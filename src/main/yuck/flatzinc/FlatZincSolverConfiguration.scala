package yuck.flatzinc

import yuck.core.{DefaultRestartLimit, DefaultSeed, Distribution, Probability}

/**
 * @author Michael Marte
 *
 */
case class FlatZincLevelConfiguration(
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
case class FlatZincSolverConfiguration(
    attachGoals: Boolean = false,
    seed: Long = DefaultSeed,
    restartLimit: Int = DefaultRestartLimit,
    numberOfThreads: Int = Runtime.getRuntime.availableProcessors,
    maybeRoundLimit: Option[Int] = None,
    maybeRuntimeLimitInSeconds: Option[Int] = None,
    maybeTargetObjectiveValue: Option[Int] = None,
    focusOnTopObjective: Boolean = true,
    stopOnFirstSolution: Boolean = false,
    pruneConstraintNetwork: Boolean = true,
    runPresolver: Boolean = true,
    useImplicitSolving: Boolean = true,
    useProgressiveTightening: Boolean = true,
    checkIncrementalCostUpdate: Boolean = false,
    checkAssignmentsToNonChannelVariables: Boolean = false,
    moveSizeDistribution: Distribution = Distribution(1, List(90, 10)),
    level0Configuration: FlatZincLevelConfiguration = FlatZincLevelConfiguration(true, Some(Probability(9))),
    level1Configuration: FlatZincLevelConfiguration = FlatZincLevelConfiguration(true, Some(Probability(13))))
{
    require(restartLimit >= 0)
    require(numberOfThreads > 0)
    require(maybeRoundLimit.getOrElse(0) >= 0)
    require(maybeRuntimeLimitInSeconds.getOrElse(0) >= 0)
}
