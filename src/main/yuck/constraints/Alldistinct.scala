package yuck.constraints

import scala.collection.*

import yuck.annealing.*
import yuck.core.*
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
    [V <: AnyValue]
    (id: Id[Constraint], override val maybeGoal: Option[Goal],
     xs: immutable.IndexedSeq[Variable[V]], costs: BooleanVariable)
    (implicit valueTraits: ValueTraits[V])
    extends ValueFrequencyTracker[V, BooleanValue](
        id, xs, costs,
        immutable.TreeMap[AnyVariable, Int](), immutable.HashMap[V, Int]())(
        valueTraits)
{

    override def toString = "alldistinct([%s], %s)".format(xs.mkString(", "), costs)

    override def propagate() = {
        if (costs.domain == TrueDomain) {
            NoPropagationOccurred.pruneDomains(
                for (x <- xs.iterator if x.domain.isSingleton;
                     y <- xs.iterator if y != x && y.domain.contains(x.domain.singleValue))
                yield (y, y.domain.diff(x.domain))
            )
        } else {
            NoPropagationOccurred
        }
    }

    override protected def computeResult(searchState: SearchState, valueRegistry: ValueRegistry) =
        BooleanValue(xs.size - valueRegistry.size)

    override def isCandidateForImplicitSolving(space: Space) = {
        val (xs, ys) = this.xs.partition(! _.domain.isSingleton)
        val as = ys.iterator.map(_.domain.singleValue).toSet
        xs.size > 1 &&
        xs.toSet.size == xs.size &&
        xs.forall(! space.isChannelVariable(_)) &&
        xs.forall(_.domain.isFinite) &&
        xs.forall(x => ! as.exists(a => x.domain.contains(a))) &&
        ys.size == as.size
    }

    override def createNeighbourhood(
        space: Space,
        randomGenerator: RandomGenerator,
        moveSizeDistribution: Distribution,
        logger: LazyLogger,
        sigint: Sigint,
        extraCfg: ExtraNeighbourhoodFactoryConfiguration):
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
                    for ((x, a) <- xs.iterator.zip(randomGenerator.shuffle(domain.values).iterator)) {
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
                val subspace =
                    new Space(logger, sigint, extraCfg.checkIncrementalCostUpdate, extraCfg.checkAssignmentsToNonChannelVariables)
                val subxs = xs.map(x => valueTraits.createVariable(subspace, x.name, x.domain))
                val result = logger.withTimedLogScope("Solving %s".format(this)) {
                    val subcosts = new BooleanVariable(subspace.nextVariableId(), "", CompleteBooleanDomain)
                    subspace.post(new Alldistinct(subspace.nextConstraintId(), maybeGoal, subxs, subcosts))
                    val initializer = new RandomInitializer(subspace, randomGenerator.nextGen())
                    initializer.run()
                    val n = subspace.searchVariables.size * 4
                    val scheduleFactory = new StandardAnnealingScheduleFactory(n, randomGenerator.nextGen())
                    val schedule = scheduleFactory.createSlowSchedule
                    schedule.start(DefaultStartTemperature, 0)
                    val solver =
                        new SimulatedAnnealing(
                            "AlldistinctSolver",
                            subspace,
                            schedule,
                            new RandomReassignmentGenerator(
                                subspace, subspace.searchVariables.toIndexedSeq, randomGenerator.nextGen(),
                                DefaultMoveSizeDistribution, maybeHotSpotDistribution = None,
                                maybeFairVariableChoiceRate = None),
                            randomGenerator.nextGen(),
                            new SatisfactionObjective(subcosts),
                            maybeRoundLimit = Some(1000),
                            Some(new StandardAnnealingMonitor(logger)),
                            maybeUserData = None,
                            sigint)
                    solver.call()
                }
                if (result.isSolution) {
                    for ((x, subx) <- xs.iterator.zip(subxs.iterator)) {
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

