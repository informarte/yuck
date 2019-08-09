package yuck.constraints

import scala.collection._
import scala.math.{abs, max, min}

import yuck.annealing._
import yuck.core._
import yuck.util.arm.Sigint
import yuck.util.logging.LazyLogger


/**
 * Used in Inverse to represent a function that should be the inverse of another function.
 *
 * @author Michael Marte
 */
final class InverseFunction
    (val xs: immutable.IndexedSeq[IntegerVariable],
     val offset: Int)
{
    require(! xs.isEmpty)
    val indexRange = offset until safeAdd(offset, xs.size)
    val indexDomain = IntegerDomain.createRange(IntegerValue.get(offset), IntegerValue.get(offset + xs.size - 1))
    val x2i = new immutable.HashMap[AnyVariable, Int] ++ xs.zip(indexRange)
    val refs = new Array[mutable.HashSet[Int]](xs.size)
    val visited = new Array[Int](xs.size)
    def isSuitableForImplicitSolving(space: Space) =
        xs.size > 1 &&
        xs.size == xs.toSet.size &&
        xs.forall(! space.isChannelVariable(_)) &&
        xs.forall(_.domain.isFinite)
}

/**
 * Used to implement the ''inverse'' constraint as specified by MiniZinc.
 *
 * Given variables f[fOffset], ..., f[fOffset + n - 1] and g[gOffset], ..., g[gOffset + m - 1],
 * the constraint computes
 *
 *  - |s(g[s(f[i])]) - i| for fOffset <= i < fOffset + n and
 *  - |s(f[s(g[j])]) - j| for gOffset <= j < gOffset + m,
 *
 * and provides the sum of these terms as measure of constraint violation.
 *
 * Out-of-bounds indices are ignored and hence both f and g may contain channel variables.
 * (In this case, additional constraints are required that force the channel variables
 * to take valid values.)
 *
 * @see [[yuck.Notation Notation]]
 *
 * @author Michael Marte
 */
final class Inverse
    (id: Id[Constraint], goal: Goal,
     f: InverseFunction,
     g: InverseFunction,
     costs: BooleanVariable)
    extends Constraint(id, goal)
{

    override def toString =
        "inverse([%s], %d, [%s], %d, %s)".format(
            f.xs.mkString(", "), f.offset, g.xs.mkString(", "), g.offset, costs)
    override def inVariables = f.xs.view ++ g.xs.view
    override def outVariables = List(costs)

    private val effects = List(new ReusableEffectWithFixedVariable[BooleanValue](costs))
    private val effect = effects.head
    private var currentCosts = 0
    private var futureCosts = 0
    private val debug = false

    private def computeCosts(
        f: InverseFunction, g: InverseFunction, i: Int, searchState: SearchState): Int =
    {
        val j = searchState.value(f.xs(i - f.offset)).value
        if (g.indexRange.contains(j)) abs(safeSub(searchState.value(g.xs(j - g.offset)).value, i))
        else 0
    }

    private def propagate(f: InverseFunction, g: InverseFunction): Boolean = {
        import IntegerDomain.createRange
        val p =
            Variable.pruneDomains(
                for (i <- f.indexRange;
                     x = f.xs(i - f.offset);
                     dx = x.domain;
                     di = createRange(IntegerValue.get(i), IntegerValue.get(i));
                     j <- g.indexDomain.diff(dx).values.view.map(_.value);
                     y = g.xs(j - g.offset))
                yield
                    (y, y.domain.diff(di)))
        val q =
            Variable.pruneDomains(
                for (i <- f.indexRange.view;
                     x = f.xs(i - f.offset);
                     dx = x.domain;
                     if dx.isSingleton;
                     y = g.xs(dx.singleValue.value - g.offset))
                yield
                    (y, y.domain.intersect(createRange(IntegerValue.get(i), IntegerValue.get(i)))))
        p || q
    }

    override def propagate =
        if (costs.domain == TrueDomain) propagate(f, g) ||| propagate(g, f) else false

    override def initialize(now: SearchState) = {
        currentCosts = 0
        for (i <- 0 until f.xs.size) {
            f.visited(i) = -1
            f.refs(i) = new mutable.HashSet[Int]
        }
        for (j <- 0 until g.xs.size) {
            g.visited(j) = -1
            g.refs(j) = new mutable.HashSet[Int]
        }
        for (i <- f.indexRange) {
            currentCosts = safeAdd(currentCosts, computeCosts(f, g, i, now))
            val j = now.value(f.xs(i - f.offset)).value
            if (g.indexRange.contains(j)) {
                g.refs(j - g.offset) += i
            }
        }
        for (j <- g.indexRange) {
            currentCosts = safeAdd(currentCosts, computeCosts(g, f, j, now))
            val i = now.value(g.xs(j - g.offset)).value
            if (f.indexRange.contains(i)) {
                f.refs(i - f.offset) += j
            }
        }
        assert(currentCosts >= 0)
        effect.a = BooleanValue.get(currentCosts)
        effects
    }

    override def consult(before: SearchState, after: SearchState, move: Move) = {
        def computeCostDelta(f: InverseFunction, g: InverseFunction, i: Int, visited: Array[Int]): Int = {
            if (! f.indexRange.contains(i) || visited(i - f.offset) == move.id.rawId) {
                0
            } else {
                visited(i - f.offset) = move.id.rawId
                val delta = safeSub(computeCosts(f, g, i, after), computeCosts(f, g, i, before))
                delta
            }
        }
        futureCosts = currentCosts
        for (x <- move) {
            val maybeI = f.x2i.get(x)
            if (maybeI.isDefined) {
                val i = maybeI.get
                val x = f.xs(i - f.offset)
                futureCosts = safeAdd(futureCosts, computeCostDelta(f, g, i, f.visited))
                for (j <- f.refs(i - f.offset)) {
                    futureCosts = safeAdd(futureCosts, computeCostDelta(g, f, j, g.visited))
                }
                futureCosts = safeAdd(futureCosts, computeCostDelta(g, f, after.value(x).value, g.visited))
            }
            val maybeJ = g.x2i.get(x)
            if (maybeJ.isDefined) {
                val j = maybeJ.get
                val y = g.xs(j - g.offset)
                futureCosts = safeAdd(futureCosts, computeCostDelta(g, f, j, g.visited))
                for (i <- g.refs(j - g.offset)) {
                    futureCosts = safeAdd(futureCosts, computeCostDelta(f, g, i, f.visited))
                }
                futureCosts = safeAdd(futureCosts, computeCostDelta(f, g, after.value(y).value, f.visited))
            }
        }
        if (debug) {
            for (i <- 0 until f.visited.size if f.visited(i) != move.id.rawId) {
                assert(computeCostDelta(f, g, i + f.offset, f.visited) == 0)
            }
            for (j <- 0 until g.visited.size if g.visited(j) != move.id.rawId) {
                assert(computeCostDelta(g, f, j + g.offset, g.visited) == 0)
            }
        }
        assert(futureCosts >= 0)
        effect.a = BooleanValue.get(futureCosts)
        effects
    }

    override def commit(before: SearchState, after: SearchState, move: Move) = {
        for (x <- move) {
            val maybeI = f.x2i.get(x)
            if (maybeI.isDefined) {
                val i = maybeI.get
                val x = f.xs(i - f.offset)
                val jBefore = before.value(x).value
                if (g.indexRange.contains(jBefore)) {
                    g.refs(jBefore - g.offset) -= i
                }
                val jAfter = after.value(x).value
                if (g.indexRange.contains(jAfter)) {
                    g.refs(jAfter - g.offset) += i
                }
            }
            val maybeJ = g.x2i.get(x)
            if (maybeJ.isDefined) {
                val j = maybeJ.get
                val y = g.xs(j - g.offset)
                val iBefore = before.value(y).value
                if (f.indexRange.contains(iBefore)) {
                    f.refs(iBefore - f.offset) -= j
                }
                val iAfter = after.value(y).value
                if (f.indexRange.contains(iAfter)) {
                    f.refs(iAfter - f.offset) += j
                }
            }
        }
        currentCosts = futureCosts
        effects
    }

    override def isCandidateForImplicitSolving(space: Space) =
        f.isSuitableForImplicitSolving(space) &&
        g.isSuitableForImplicitSolving(space) &&
        f.xs.size == g.xs.size

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
            if (f.xs.toSet.intersect(g.xs.toSet).isEmpty) {
                if (f.xs.forall(x => x.domain == g.indexDomain) &&
                    g.xs.forall(x => x.domain == f.indexDomain))
                {
                    // simplest case
                    for ((x, j) <- f.xs.iterator.zip(g.indexRange.iterator)) {
                        space.setValue(x, IntegerValue.get(j))
                    }
                    for ((y, i) <- g.xs.iterator.zip(f.indexRange.iterator)) {
                        space.setValue(y, IntegerValue.get(i))
                    }
                    space.setValue(costs, True)
                    Some(new SimpleInverseNeighbourhood(space, f, g, randomGenerator))
                } else {
                    // general case
                    val logger = space.logger
                    val subspace = new Space(logger, space.checkIncrementalCostUpdate)
                    val subf = new InverseFunction(f.xs.map(x => new IntegerVariable(subspace.nextVariableId, x.name, x.domain)), f.offset)
                    val subg = new InverseFunction(g.xs.map(y => new IntegerVariable(subspace.nextVariableId, y.name, y.domain)), g.offset)
                    val result = logger.withTimedLogScope("Solving %s".format(this)) {
                        val subcosts = new BooleanVariable(subspace.nextVariableId, "", CompleteBooleanDomain)
                        subspace.post(new Inverse(subspace.nextConstraintId, goal, subf, subg, subcosts))
                        val initializer = new RandomInitializer(subspace, randomGenerator.nextGen)
                        initializer.run
                        val n = subspace.searchVariables.size * 4
                        val scheduleFactory = new StandardAnnealingScheduleFactory(n, randomGenerator.nextGen)
                        val schedule = scheduleFactory.createSlowSchedule
                        schedule.start(DefaultStartTemperature, 0)
                        val solver =
                            new SimulatedAnnealing(
                                "InverseSolver",
                                subspace,
                                schedule,
                                new RandomReassignmentGenerator(
                                    subspace, subspace.searchVariables.toIndexedSeq, randomGenerator.nextGen,
                                    DefaultMoveSizeDistribution, maybeHotSpotDistribution = None,
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
                        for ((x, subx) <- f.xs.iterator.zip(subf.xs.iterator)) {
                            space.setValue(x, subspace.searchState.value(subx))
                        }
                        for ((y, suby) <- g.xs.iterator.zip(subg.xs.iterator)) {
                            space.setValue(y, subspace.searchState.value(suby))
                        }
                        space.setValue(costs, True)
                        Some(new GeneralInverseNeighbourhood(space, f, g, randomGenerator))
                    } else {
                        None
                    }
                }
            } else if (f.xs.size % 2 == 0 && f.xs.size > 2 && f.xs == g.xs &&
                       f.offset == g.offset && f.xs.forall(x => x.domain == f.indexDomain))
            {
                // self-inverse case, occurs in elitserien (look for RRT 5)
                for (IndexedSeq(i1, i2) <- f.indexRange.grouped(2)) {
                    space.setValue(f.xs(i1 - f.offset), IntegerValue.get(i2))
                    space.setValue(f.xs(i2 - f.offset), IntegerValue.get(i1))
                }
                space.setValue(costs, True)
                Some(new SelfInverseNeighbourhood(space, f, randomGenerator))
            } else {
                None
            }
        } else {
            None
        }
    }

    // Sometimes inverse constraints are decomposable (see elitserien, for example).
    def decompose(space: Space): Seq[Inverse] = {
        type Partition = mutable.ArrayBuffer[IntegerVariable]
        type PartitionByDomain = mutable.AnyRefMap[IntegerDomain, Partition]
        def partitionByDomain(xs: Iterable[IntegerVariable]): PartitionByDomain =
            xs.foldLeft(new PartitionByDomain()) {
                (map, x) => {
                    val dx = x.domain
                    map += dx -> (map.getOrElse(dx, new Partition()) += x)
                }
            }
        lazy val fPartitionByDomain = partitionByDomain(f.xs)
        lazy val gPartitionByDomain = partitionByDomain(g.xs)
        def domainLt(lhs: IntegerDomain, rhs: IntegerDomain) =
            lhs.lb < rhs.lb || (lhs.lb == rhs.lb && lhs.ub < rhs.ub)
        def union(lhs: IntegerDomain, rhs: IntegerDomain) = lhs.union(rhs)
        val isDecomposable =
            // the offsets are equal
            f.offset == g.offset &&
            // the variables in f have the same domains as the variables in g
            fPartitionByDomain.keysIterator.toBuffer.sortWith(domainLt) ==
                gPartitionByDomain.keysIterator.toBuffer.sortWith(domainLt) &&
            // the variables do not all have the same domain
            fPartitionByDomain.size > 1 &&
            // the domains do not overlap
            fPartitionByDomain.keysIterator.foldLeft[IntegerDomain](EmptyIntegerRange){union}.size ==
                fPartitionByDomain.keysIterator.map(_.size).sum
        if (isDecomposable) {
            for (domain <- fPartitionByDomain.keysIterator.toList) yield {
                val offset = domain.lb.value
                val costs = new BooleanVariable(space.nextVariableId, "", CompleteBooleanDomain)
                new Inverse(
                    space.nextConstraintId, goal,
                    new InverseFunction(fPartitionByDomain(domain).toIndexedSeq, offset),
                    new InverseFunction(gPartitionByDomain(domain).toIndexedSeq, offset),
                    costs)
            }
        } else {
            List(this)
        }
    }

}

abstract class InverseNeighbourhood
    (protected val space: Space,
     protected val f: InverseFunction,
     protected val g: InverseFunction)
    extends Neighbourhood
{

    protected final def value(x: IntegerVariable): IntegerValue = space.searchState.value(x)
    protected final def rawValue(x: IntegerVariable): Int = value(x).value

    require(f.xs.size == g.xs.size)
    require(f.isSuitableForImplicitSolving(space))
    require(g.isSuitableForImplicitSolving(space))
    require(f.xs.forall(x => x.domain.isSubsetOf(g.indexDomain)))
    require(g.xs.forall(x => x.domain.isSubsetOf(f.indexDomain)))
    require(f.indexRange.forall(i => rawValue(g.xs(rawValue(f.xs(i - f.offset)) - g.offset)) == i))

    final override def searchVariables = f.xs.toSet

    final override def children = Nil

}

/**
 * This neighbourhood can be used to maintain an ''inverse'' constraint where
 *
 *  - f and g do not share variables
 *  - every x in f can be paired with every y in g and vice versa.
 *
 * @author Michael Marte
 */
final class SimpleInverseNeighbourhood
    (space: Space, f: InverseFunction, g: InverseFunction, randomGenerator: RandomGenerator)
    extends InverseNeighbourhood(space, f, g)
{

    require(f.xs.toSet.intersect(g.xs.toSet).isEmpty)
    require(f.xs.forall(x => x.domain == g.indexDomain))
    require(g.xs.forall(x => x.domain == f.indexDomain))

    private val n = f.xs.size
    private val effects = Vector.fill(4){new ReusableEffect[IntegerValue]}

    override def nextMove = {
        val i1 = randomGenerator.nextInt(n)
        val x1 = f.xs(i1)
        val a1 = value(x1)
        val j1 = a1.value - g.offset
        val y1 = g.xs(j1)
        val b1 = value(y1)
        assert(b1.value - f.offset == i1)
        val i2 = {
            val i = randomGenerator.nextInt(n - 1)
            if (i < i1) i else i + 1
        }
        val x2 = f.xs(i2)
        val a2 = value(x2)
        val j2 = a2.value - g.offset
        val y2 = g.xs(j2)
        val b2 = value(y2)
        assert(b2.value - f.offset == i2)
        // {(x1, y1), (x2, y2)} -> {(x1, y2), (x2, y1)}
        effects(0).set(x1, a2)
        effects(1).set(x2, a1)
        effects(2).set(y1, b2)
        effects(3).set(y2, b1)
        new ChangeValues(space.nextMoveId, effects)
    }

}

/**
 * This neighbourhood can be used to maintain an ''inverse'' constraint where
 * f and g do not share variables.
 *
 * @author Michael Marte
 */
final class GeneralInverseNeighbourhood
    (space: Space, f: InverseFunction, g: InverseFunction, randomGenerator: RandomGenerator)
    extends InverseNeighbourhood(space, f, g)
{

    require(f.xs.toSet.intersect(g.xs.toSet).isEmpty)

    private val effects = Vector.fill(4){new ReusableEffect[IntegerValue]}
    private val candidates1 =
         (0 until f.xs.size)
        .filter(i => f.xs(i).domain.size > 1 && g.xs(rawValue(f.xs(i)) - g.offset).domain.size > 1)

    override def nextMove = {
        if (candidates1.isEmpty) {
            new ChangeValues[IntegerValue](space.nextMoveId, Nil)
        } else {
            val i1 = candidates1(randomGenerator.nextInt(candidates1.size))
            val x1 = f.xs(i1)
            val a1 = value(x1)
            val j1 = a1.value - g.offset
            val y1 = g.xs(j1)
            val b1 = value(y1)
            assert(b1.value - f.offset == i1)
            val candidates2 =
                y1
                .domain
                .values
                .iterator
                .map(_.value - f.offset)
                .filter(i2 => i2 != i1)
                .filter(
                    i2 => {
                        val x2 = f.xs(i2)
                        val a2 = value(x2)
                        val j2 = a2.value - g.offset
                        val y2 = g.xs(j2)
                        val b2 = value(y2)
                        assert(b2.value - f.offset == i2)
                        x1.domain.contains(a2) && x2.domain.contains(a1) &&
                        y1.domain.contains(b2) && y2.domain.contains(b1)
                    })
                .toIndexedSeq
            if (candidates2.isEmpty) {
                new ChangeValues[IntegerValue](space.nextMoveId, Nil)
            } else {
                val i2 = candidates2(randomGenerator.nextInt(candidates2.size))
                val x2 = f.xs(i2)
                val a2 = value(x2)
                val j2 = a2.value - g.offset
                val y2 = g.xs(j2)
                val b2 = value(y2)
                assert(b2.value - f.offset == i2)
                // {(x1, y1), (x2, y2)} -> {(x1, y2), (x2, y1)}
                effects(0).set(x1, a2)
                effects(1).set(x2, a1)
                effects(2).set(y1, b2)
                effects(3).set(y2, b1)
                new ChangeValues(space.nextMoveId, effects)
            }
        }
    }

}

/**
 * This neighbourhood can be used to maintain an ''inverse'' constraint with
 * f = g.
 *
 * @author Michael Marte
 */
final class SelfInverseNeighbourhood
    (space: Space, f: InverseFunction, randomGenerator: RandomGenerator)
    extends InverseNeighbourhood(space, f, f)
{

    private val n = f.xs.size

    require(n > 2)
    require(n % 2 == 0)
    require(f.xs.forall(x => x.domain == f.indexDomain))

    private val effects = Vector.fill(4){new ReusableEffect[IntegerValue]}

    override def nextMove = {
        val i1 = randomGenerator.nextInt(n)
        val x1 = f.xs(i1)
        val a1 = value(x1)
        val i2 = a1.value - f.offset
        val x2 = f.xs(i2)
        val a2 = value(x2)
        assert(a2.value - f.offset == i1)
        val i3 = {
            var i = randomGenerator.nextInt(n - 2)
            if (i < min(i1, i2)) i else if (i > max(i1, i2) - 2) i + 2 else i + 1
        }
        val x3 = f.xs(i3)
        val a3 = value(x3)
        val i4 = a3.value - f.offset
        val x4 = f.xs(i4)
        val a4 = value(x4)
        assert(a4.value - f.offset == i3)
        // {(x1, x2), (x3, x4)} -> {(x1, x3), (x2, x4)}
        effects(0).set(x1, a3)
        effects(1).set(x3, a1)
        effects(2).set(x2, a4)
        effects(3).set(x4, a2)
        new ChangeValues(space.nextMoveId, effects)
    }

}
