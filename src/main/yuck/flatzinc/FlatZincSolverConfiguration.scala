package yuck.flatzinc

import yuck.core.{DefaultRestartLimit, DefaultSeed, Distribution, Probability}

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
    attachGoals: Boolean = false,
    seed: Long = DefaultSeed,
    restartLimit: Int = DefaultRestartLimit,
    numberOfThreads: Int = Runtime.getRuntime.availableProcessors,
    maybeRoundLimit: Option[Int] = None,
    maybeRuntimeLimitInSeconds: Option[Int] = None,
    maybeTargetObjectiveValue: Option[Long] = None,
    focusOnTopObjective: Boolean = true,
    stopOnFirstSolution: Boolean = false,
    pruneConstraintNetwork: Boolean = true,
    runPresolver: Boolean = true,
    useImplicitSolving: Boolean = true,
    useProgressiveTightening: Boolean = true,
    checkIncrementalCostUpdate: Boolean = false,
    checkAssignmentsToNonChannelVariables: Boolean = false,
    moveSizeDistribution: Distribution = Distribution(1, List(90, 10)),
    topLevelConfiguration: FlatZincLevelConfiguration = FlatZincLevelConfiguration(true, true, Some(Probability(9))),
    subordinateLevelConfiguration: FlatZincLevelConfiguration = FlatZincLevelConfiguration(false, true, Some(Probability(13))))
{
    require(restartLimit >= 0)
    require(numberOfThreads > 0)
    require(maybeRoundLimit.getOrElse(0) >= 0)
    require(maybeRuntimeLimitInSeconds.getOrElse(0) >= 0)
    require(topLevelConfiguration.isTopLevel)
    require(! subordinateLevelConfiguration.isTopLevel)
}
