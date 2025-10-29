package yuck.flatzinc

import yuck.annealing.{DefaultPerturbationProbability, DefaultStartTemperature, DefaultWarmStartTemperature}
import yuck.core.profiling.SpaceProfilingMode
import yuck.core.{Constraint, DefaultRestartLimit, DefaultSeed, Distribution, Probability}

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
final case class FlatZincSolverConfiguration(
    maybeName: Option[String] = None,
    attachGoals: Boolean = false,
    seed: Long = DefaultSeed,
    numberOfSolvers: Int = 1,
    numberOfThreads: Int = Runtime.getRuntime.availableProcessors,
    maybeRoundLimit: Option[Int] = None,
    maybeRuntimeLimitInSeconds: Option[Int] = None,
    maybeTargetObjectiveValue: Option[Long] = None,
    focusOnTopObjective: Boolean = true,
    stopOnFirstSolution: Boolean = false,
    optimizeArrayAccess: Boolean = true,
    pruneConstraintNetwork: Boolean = true,
    runPresolver: Boolean = true,
    useImplicitSolving: Boolean = true,
    useProgressiveTightening: Boolean = true,
    checkIncrementalCostUpdate: Constraint => Boolean = _ => false,
    checkAssignmentsToNonChannelVariables: Boolean = false,
    delayCycleCheckingUntilInitialization: Boolean = false,
    startTemperature: Double = DefaultStartTemperature,
    warmStartTemperature: Double = DefaultWarmStartTemperature,
    perturbationProbability: Probability = DefaultPerturbationProbability,
    maybeSpaceProfilingMode: Option[SpaceProfilingMode] = None,
    moveSizeDistribution: Distribution = Distribution(1, List(90, 10)),
    topLevelConfiguration: FlatZincLevelConfiguration = FlatZincLevelConfiguration(true, true, Some(Probability(9))),
    subordinateLevelConfiguration: FlatZincLevelConfiguration = FlatZincLevelConfiguration(false, true, Some(Probability(13))))
{
    require(numberOfSolvers > 0)
    require(numberOfThreads > 0)
    require(maybeRoundLimit.getOrElse(0) >= 0)
    require(maybeRuntimeLimitInSeconds.getOrElse(0) >= 0)
    require(topLevelConfiguration.isTopLevel)
    require(! subordinateLevelConfiguration.isTopLevel)
}
