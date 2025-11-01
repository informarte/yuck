package yuck.test

import yuck.annealing.*
import yuck.constraints.SatisfactionGoalTracker
import yuck.constraints.SatisfactionGoalTracker.computeInvolvementMap
import yuck.core.*
import yuck.{annealing, fj}
import yuck.fj.{FeasibilityJump, FeasibilityJumpEventLogger, FeasibilityJumpNeighbourhood}
import yuck.test.util.IntegrationTest

/**
 * @author Michael Marte
 *
 */
abstract class HelloWorldTest extends IntegrationTest {

    protected def createSimulatedAnnealingSolver
        (name: String,
         space: Space,
         objective: AnyObjective,
         neighbourhood: Neighbourhood,
         randomGenerator: RandomGenerator,
         maybeUserData: Option[Object]):
        SimulatedAnnealing =
    {
        val scheduleFactory = new AnnealingScheduleFactory(space.searchVariables.size, randomGenerator.nextGen())
        val schedule = scheduleFactory.createHybridSchedule()
        new SimulatedAnnealing(
            name,
            space,
            objective,
            neighbourhood,
            schedule,
            annealing.DefaultStartTemperature,
            annealing.DefaultStartTemperature,
            annealing.DefaultPerturbationProbability,
            None,
            randomGenerator.nextGen(),
            None,
            Some(new AnnealingEventLogger(logger)),
            maybeUserData,
            sigint)
    }

    protected def createFeasibilityJumpSolver
        (name: String,
         space: Space,
         xs: Vector[AnyVariable],
         cs: Vector[BooleanVariable],
         objective: AnyObjective,
         randomGenerator: RandomGenerator,
         maybeUserData: Option[Object]):
        FeasibilityJump =
    {
        val n = xs.size
        cs.foreach(space.registerObjectiveVariable)
        val involvementMap = computeInvolvementMap(space, xs, cs)
        val hotSpotDistribution = Distribution(n)
        space.post(new SatisfactionGoalTracker(space.nextConstraintId(), None, involvementMap, hotSpotDistribution))
        val neighbourhood = new FeasibilityJumpNeighbourhood(
            space,
            xs,
            cs,
            involvementMap,
            hotSpotDistribution,
            randomGenerator.nextGen(),
            fj.DefaultMoveSizeDistribution,
            fj.defaultMaximumNumberOfJumpCandidates(n),
            false,
            fj.defaultNumberOfValuesToExplore,
            fj.DefaultMinimumJumpValueCacheHitRate,
            fj.DefaultWeightDecayRate,
            true,
            true)
        new FeasibilityJump(
            name,
            space,
            objective,
            neighbourhood,
            fj.defaultNumberOfMovesPerRound(n),
            fj.DefaultNumberOfSuccessiveFutileRoundsUntilPerturbation,
            fj.DefaultPerturbationProbability,
            None,
            randomGenerator.nextGen(),
            Some(new FeasibilityJumpEventLogger(logger)),
            maybeUserData,
            sigint)
    }

}
