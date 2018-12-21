package yuck.constraints

import scala.collection._
import scala.math.{max, min}

import yuck.annealing._
import yuck.core._
import yuck.util.arm.Sigint
import yuck.util.logging.LazyLogger



/**
 * Implements the ''all_different_int'' constraint as specified by MiniZinc.
 *
 * Given a set X of variables, the constraint maintains the set A = {s(x): x in X} of values
 * assigned to the variables and provides |X| - |A| as measure of constraint violation.
 *
 * @see [[yuck.Notation Notation]]
 *
 * @author Michael Marte
 */
final class Alldistinct
    [Value <: AnyValue]
    (id: Id[Constraint], goal: Goal,
     xs: immutable.IndexedSeq[Variable[Value]], costs: BooleanVariable)
    (implicit valueTraits: AnyValueTraits[Value])
    extends ValueFrequencyTracker[Value, BooleanValue](
        id, goal, xs, costs,
        immutable.TreeMap[AnyVariable, Int](), immutable.HashMap[Value, Int]())(
        valueTraits)
{

    override def toString = "alldistinct([%s], %s)".format(xs.mkString(", "), costs)

    override def propagate = {
        if (costs.domain == TrueDomain) {
            Variable.pruneDomains(
                for (x <- xs.toIterator if x.domain.isSingleton;
                     y <- xs.toIterator if y != x && y.domain.contains(x.domain.singleValue))
                yield (y, y.domain.diff(x.domain))
            )
        } else {
            false
        }
    }

    override protected def computeResult(searchState: SearchState, valueRegistry: ValueRegistry) =
        BooleanValue.get(xs.size - valueRegistry.size)

    override def isCandidateForImplicitSolving(space: Space) = {
        val (xs, ys) = this.xs.partition(! _.domain.isSingleton)
        val as = ys.toIterator.map(_.domain.singleValue).toSet
        ys.size == as.size &&
        xs.size > 1 &&
        xs.toSet.size == xs.size &&
        xs.forall(! space.isChannelVariable(_)) &&
        xs.forall(_.domain.isFinite) &&
        xs.forall(x => ! as.exists(a => x.domain.contains(a)))
    }

    override def prepareForImplicitSolving(
        space: Space,
        randomGenerator: RandomGenerator,
        moveSizeDistribution: Distribution,
        hotSpotDistributionFactory: Seq[AnyVariable] => Option[Distribution],
        maybeFairVariableChoiceRate: Option[Probability],
        sigint: Sigint):
        Option[Neighbourhood] =
    {
        if (isCandidateForImplicitSolving(space)) {
            val xs = this.xs.filter(! _.domain.isSingleton)
            if (xs.forall(_.domain == xs.head.domain)) {
                // all variables have the same domain
                if (xs.head.domain.size >= xs.size) {
                    // the number of values equals the number of variables or
                    // there are more values than variables
                    val domain = xs.head.domain
                    for ((x, a) <- xs.toIterator.zip(randomGenerator.shuffle(domain.values).toIterator)) {
                        space.setValue(x, a)
                    }
                    space.setValue(costs, True)
                    Some(new AlldistinctNeighbourhood(space, xs, randomGenerator, moveSizeDistribution))
                } else {
                    // unsatisfiable
                    None
                }
            } else {
                // general case
                val logger = space.logger
                val subspace = new Space(logger, space.checkConstraintPropagation)
                val subxs = xs.map(x => valueTraits.createVariable(subspace, x.name, x.domain))
                val result = logger.withTimedLogScope("Solving %s".format(this)) {
                    val subcosts = new BooleanVariable(subspace.variableIdFactory.nextId, "", CompleteBooleanDomain)
                    subspace.post(new Alldistinct(subspace.constraintIdFactory.nextId, goal, subxs, subcosts))
                    val initializer = new RandomInitializer(subspace, randomGenerator.nextGen)
                    initializer.run
                    val n = subspace.searchVariables.size * 4
                    val scheduleFactory = new StandardAnnealingScheduleFactory(n, randomGenerator.nextGen)
                    val schedule = scheduleFactory.createSlowSchedule
                    scheduleFactory.startScheduleWithRandomTemperature(schedule)
                    val solver =
                        new SimulatedAnnealing(
                            "AlldistinctSolver",
                            subspace,
                            schedule,
                            new RandomReassignmentGenerator(
                                subspace, subspace.searchVariables.toIndexedSeq, randomGenerator.nextGen,
                                DEFAULT_MOVE_SIZE_DISTRIBUTION, maybeHotSpotDistribution = None,
                                maybeFairVariableChoiceRate = None),
                            randomGenerator.nextGen,
                            new MinimizationObjective(subcosts, True, None),
                            maybeRoundLimit = Some(1000),
                            Some(new StandardAnnealingMonitor(logger)),
                            maybeUserData = None,
                            sigint)
                    solver.call
                }
                if (result.isSolution) {
                    for ((x, subx) <- xs.toIterator.zip(subxs.toIterator)) {
                        space.setValue(x, subspace.searchState.value(subx))
                    }
                    space.setValue(costs, True)
                    Some(new AlldistinctNeighbourhood(space, xs, randomGenerator, moveSizeDistribution))
                } else {
                    None
                }
            }
        } else {
            None
        }
    }

}

/**
 * This neighbourhood can be used to maintain any ''all_different_int'' constraint.
 *
 * @author Michael Marte
 */
final class AlldistinctNeighbourhood
    [Value <: AnyValue]
    (space: Space,
     xs: immutable.IndexedSeq[Variable[Value]],
     randomGenerator: RandomGenerator,
     moveSizeDistribution: Distribution)
    (implicit valueTraits: AnyValueTraits[Value])
    extends Neighbourhood
{

    private val n = xs.size
    private def value(x: Variable[Value]) = space.searchState.value(x)

    require(n > 1)
    require(xs.toSet.size == n)
    require(xs.map(value).toSet.size == n)
    require(moveSizeDistribution.frequency(0) == 0)
    require(moveSizeDistribution.volume > 0)

    private val probabilityOfSwappingInValues = moveSizeDistribution.probability(1)
    private val variablesHaveTheSameDomain = xs.forall(x => x.domain == xs.head.domain)
    private val swappingInValuesIsPossible = xs.toIterator.map(_.domain.values).flatten.toSet.size > n
    private val effects = Vector.fill(3){new ReusableEffect[Value]}
    private val swaps = for (n <- 1 to 3) yield effects.take(n)
    private def succeed(n: Int): Move = new ChangeValues[Value](space.moveIdFactory.nextId, swaps(n - 1))
    private def fail: Move = new ChangeValues[Value](space.moveIdFactory.nextId, Nil)

    private def nextMove(m: Int): Move = {
        require(m <= n)
        val swapInAValue =
            swappingInValuesIsPossible &&
            (m == 1 || randomGenerator.nextDecision(probabilityOfSwappingInValues))
        if (swapInAValue) {
            val usedValues = valueTraits.createDomain(xs.toIterator.map(value).toSet)
            if (m == 1) {
                val candidates =
                    randomGenerator
                    .lazyShuffle(xs)
                    .map(x => (x, (x.domain.diff(usedValues))))
                    .filter(! _._2.isEmpty)
                val (x, unusedValues) = candidates.next
                val u = unusedValues.randomValue(randomGenerator)
                // {(x, a)} -> {(x, u)}
                effects(0).set(x, u)
                succeed(1)
            } else if (variablesHaveTheSameDomain) {
                val candidates =
                    randomGenerator
                    .lazyShuffle(xs.indices)
                    .map(i => (i, (xs(i).domain.diff(usedValues))))
                    .filter(! _._2.isEmpty)
                val (i, unusedValues) = candidates.next
                val x = xs(i)
                val a = value(x)
                val u = unusedValues.randomValue(randomGenerator)
                val j = {
                    val k = randomGenerator.nextInt(n - 1)
                    if (k < i) k else k + 1
                }
                val y = xs(j)
                if (m == 2) {
                    // {(x, a), (y, b)} -> {(x, u), (y, a)}
                    effects(0).set(x, u)
                    effects(1).set(y, a)
                    succeed(2)
                } else {
                    val b = value(y)
                    val k = {
                        val l = randomGenerator.nextInt(n - 2)
                        if (l < min(i, j)) l else if (l > max(i, j) - 2) l + 2 else l + 1
                    }
                    val z = xs(k)
                    // {(x, a), (y, b), (z, c)} -> {(x, u), (y, a), (z, b)}
                    effects(0).set(x, u)
                    effects(1).set(y, a)
                    effects(2).set(z, b)
                    succeed(3)
                }
            } else if (m == 2) {
                val candidates =
                    for {
                        x <- randomGenerator.lazyShuffle(xs)
                        unusedValues = x.domain.diff(usedValues)
                        if ! unusedValues.isEmpty
                        a = value(x)
                        ys = xs.filter(y => y != x && y.domain.contains(a))
                        y <- randomGenerator.lazyShuffle(ys)
                    } yield {
                        (x, unusedValues, y)
                    }
                if (candidates.hasNext){
                    val (x, unusedValues, y) = candidates.next
                    val a = value(x)
                    val u = unusedValues.randomValue(randomGenerator)
                    // {(x, a), (y, b)} -> {(x, u), (y, a)}
                    effects(0).set(x, u)
                    effects(1).set(y, a)
                    succeed(2)
                } else {
                    nextMove(1)
                }
            } else {
                val candidates =
                    for {
                        x <- randomGenerator.lazyShuffle(xs)
                        unusedValues = x.domain.diff(usedValues)
                        if ! unusedValues.isEmpty
                        a = value(x)
                        ys = xs.filter(y => y != x && y.domain.contains(a))
                        y <- randomGenerator.lazyShuffle(ys)
                        b = value(y)
                        zs = xs.filter(z => x != z && y != z && z.domain.contains(b))
                        z <- randomGenerator.lazyShuffle(zs)
                    } yield {
                        (x, unusedValues, y, z)
                    }
                if (candidates.hasNext) {
                    val (x, unusedValues, y, z) = candidates.next
                    val (a, b) = (value(x), value(y))
                    val u = unusedValues.randomValue(randomGenerator)
                    // {(x, a), (y, b), (z, c)} -> {(x, u), (y, a), (z, b)}
                    effects(0).set(x, u)
                    effects(1).set(y, a)
                    effects(2).set(z, b)
                    succeed(3)
                } else {
                    nextMove(2)
                }
            }
        } else if (variablesHaveTheSameDomain) {
            val i = randomGenerator.nextInt(n)
            val x = xs(i)
            val a = value(x)
            val j = {
                val k = randomGenerator.nextInt(n - 1)
                if (k < i) k else k + 1
            }
            val y = xs(j)
            val b = value(y)
            if (m == 2) {
                // {(x, a), (y, b)} -> {(x, b), (y, a)}
                effects(0).set(x, b)
                effects(1).set(y, a)
                succeed(2)
            } else {
                val k = {
                    val l = randomGenerator.nextInt(n - 2)
                    if (l < min(i, j)) l else if (l > max(i, j) - 2) l + 2 else l + 1
                }
                val z = xs(k)
                val c = value(z)
                // {(x, a), (y, b), (z, c)} -> {(x, c), (y, a), (z, b)}
                effects(0).set(x, c)
                effects(1).set(y, a)
                effects(2).set(z, b)
                succeed(3)
            }
        } else if (m == 2) {
            val candidates =
                for {
                    x <- randomGenerator.lazyShuffle(xs)
                    a = value(x)
                    ys = xs.filter(y => y != x && x.domain.contains(value(y)) && y.domain.contains(a))
                    y <- randomGenerator.lazyShuffle(ys)
                } yield {
                    (x, y)
                }
            if (candidates.hasNext) {
                val (x, y) = candidates.next
                val (a, b) = (value(x), value(y))
                // {(x, a), (y, b)} -> {(x, b), (y, a)}
                effects(0).set(x, b)
                effects(1).set(y, a)
                succeed(2)
            } else if (swappingInValuesIsPossible) {
                nextMove(1)
            } else {
                fail
            }
        } else {
            val candidates =
                for {
                    x <- randomGenerator.lazyShuffle(xs)
                    a = value(x)
                    ys = xs.filter(y => y != x && y.domain.contains(a))
                    y <- randomGenerator.lazyShuffle(ys)
                    b = value(y)
                    zs = xs.filter(z => x != z && y != z && x.domain.contains(value(z)) && z.domain.contains(b))
                    z <- randomGenerator.lazyShuffle(zs)
                } yield {
                    (x, y, z)
                }
            if (candidates.hasNext) {
                val (x, y, z) = candidates.next
                val (a, b, c) = (value(x), value(y), value(z))
                // {(x, a), (y, b), (z, c)} -> {(x, c), (y, a), (z, b)}
                effects(0).set(x, c)
                effects(1).set(y, a)
                effects(2).set(z, b)
                succeed(3)
            } else {
                nextMove(2)
            }
        }
    }

    override def searchVariables = xs.toSet

    override def children = Nil

    override def nextMove = {
        val m1 = scala.math.min(n, moveSizeDistribution.nextIndex(randomGenerator))
        val m2 = if (swappingInValuesIsPossible) m1 else scala.math.max(m1, 2)
        nextMove(m2)
    }

}
