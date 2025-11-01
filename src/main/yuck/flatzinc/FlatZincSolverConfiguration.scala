package yuck.flatzinc

import yuck.{SolvingMethod, annealing, fj}
import yuck.core.profiling.SpaceProfilingMode
import yuck.core.{Constraint, DefaultSeed, Distribution, Probability}

/**
 * @author Michael Marte
 *
 */
final case class FlatZincLevelConfiguration(
    isTopLevel: Boolean,
    guideOptimization: Boolean,
    maybeFairVariableChoiceRate: Option[Probability])
{
    require(! guideOptimization || maybeFairVariableChoiceRate.getOrElse(Probability(0)).value < 1)
    require(maybeFairVariableChoiceRate.isEmpty || guideOptimization)
}

/**
 * @author Michael Marte
 *
 */
final case class AnnealingConfiguration(
    maybeRoundLimit: Option[Int] = None,
    useImplicitSolving: Boolean = true,
    startTemperature: Double = annealing.DefaultStartTemperature,
    warmStartTemperature: Double = annealing.DefaultWarmStartTemperature,
    perturbationProbability: Probability = annealing.DefaultPerturbationProbability,
    moveSizeDistribution: Distribution = annealing.DefaultMoveSizeDistribution,
    topLevelConfiguration: FlatZincLevelConfiguration = FlatZincLevelConfiguration(true, true, Some(Probability(9))),
    subordinateLevelConfiguration: FlatZincLevelConfiguration = FlatZincLevelConfiguration(false, true, Some(Probability(13))))
{
    require(maybeRoundLimit.getOrElse(0) >= 0)
    require(topLevelConfiguration.isTopLevel)
    require(! subordinateLevelConfiguration.isTopLevel)
}

/**
 * @author Michael Marte
 *
 */
final case class FeasibilityJumpConfiguration(
    moveSizeDistribution: Distribution = fj.DefaultMoveSizeDistribution,
    maximumNumberOfJumpCandidates: Int => Int = fj.defaultMaximumNumberOfJumpCandidates,
    useConvexArgMin: Boolean = false,
    numberOfValuesToExplore: Int => Int = fj.defaultNumberOfValuesToExplore,
    minimumJumpValueCacheHitRate: Double = fj.DefaultMinimumJumpValueCacheHitRate,
    weightDecayRate: Double = fj.DefaultWeightDecayRate,
    numberOfMovesPerRound: Int => Int = fj.defaultNumberOfMovesPerRound,
    numberOfSuccessiveFutileRoundsUntilPerturbation: Int = fj.DefaultNumberOfSuccessiveFutileRoundsUntilPerturbation,
    perturbationProbability: Probability = fj.DefaultPerturbationProbability,
    resetWeightsOnPerturbation: Boolean = true,
    scaleCostDeltas: Boolean = true)
{
    require(minimumJumpValueCacheHitRate >= 0 && minimumJumpValueCacheHitRate <= 1)
    require(weightDecayRate >= 0 && weightDecayRate <= 1)
    require(numberOfSuccessiveFutileRoundsUntilPerturbation >= 1)
}

/**
 * @author Michael Marte
 *
 */
final case class FlatZincSolverConfiguration(
    name: String = "",
    attachGoals: Boolean = false,
    seed: Long = DefaultSeed,
    numberOfSolvers: Int = 2,
    numberOfThreads: Int = Runtime.getRuntime.availableProcessors,
    maybePreferredSolvingMethod: Option[SolvingMethod] = None,
    maybeRuntimeLimitInSeconds: Option[Int] = None,
    maybeTargetObjectiveValue: Option[Long] = None,
    focusOnTopObjective: Boolean = true,
    stopOnFirstSolution: Boolean = false,
    optimizeArrayAccess: Boolean = true,
    pruneConstraintNetwork: Boolean = true,
    runPresolver: Boolean = true,
    useProgressiveTightening: Boolean = true,
    shareBounds: Boolean = true,
    checkIncrementalCostUpdate: Constraint => Boolean = _ => false,
    checkAssignmentsToNonChannelVariables: Boolean = false,
    delayCycleCheckingUntilInitialization: Boolean = false,
    maybeSpaceProfilingMode: Option[SpaceProfilingMode] = None,
    annealingConfiguration: AnnealingConfiguration = AnnealingConfiguration(),
    feasibilityJumpConfiguration: FeasibilityJumpConfiguration = FeasibilityJumpConfiguration())
{
    require(numberOfSolvers > 0)
    require(numberOfThreads > 0)
    require(maybeRuntimeLimitInSeconds.getOrElse(0) >= 0)
}
