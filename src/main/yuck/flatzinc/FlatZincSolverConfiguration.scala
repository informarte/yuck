package yuck.flatzinc

import yuck.core.{DefaultRestartLimit, DefaultSeed, Distribution, DistributionFactory, Probability}

/**
 * @author Michael Marte
 *
 */
case class FlatZincLevelConfiguration(
    val guideOptimization: Boolean,
    val maybeFairVariableChoiceRate: Option[Probability])
{
    require(! guideOptimization || maybeFairVariableChoiceRate.getOrElse(Probability.from(0)).value < 1)
    require(maybeFairVariableChoiceRate.isEmpty || guideOptimization)
}

/**
 * @author Michael Marte
 *
 */
case class FlatZincSolverConfiguration(
    val seed: Long = DefaultSeed,
    val restartLimit: Int = DefaultRestartLimit,
    val numberOfThreads: Int = Runtime.getRuntime.availableProcessors,
    val maybeRoundLimit: Option[Int] = None,
    val maybeRuntimeLimitInSeconds: Option[Int] = None,
    val maybeTargetObjectiveValue: Option[Int] = None,
    val maybeQualityTolerance: Option[Int] = None,
    val stopOnFirstSolution: Boolean = false,
    val useProgressiveTightening: Boolean = true,
    val preferImplicitSolvingOverDomainPruning: Boolean = false,
    val checkIncrementalCostUpdate: Boolean = false,
    val checkAssignmentsToNonChannelVariables: Boolean = false,
    val moveSizeDistribution: Distribution = DistributionFactory.createDistribution(1, List(90, 10)),
    val level0Configuration: FlatZincLevelConfiguration = FlatZincLevelConfiguration(true, Some(Probability.from(3))),
    val level1Configuration: FlatZincLevelConfiguration = FlatZincLevelConfiguration(true, Some(Probability.from(13))))
{
    require(restartLimit >= 0)
    require(numberOfThreads > 0)
    require(maybeRoundLimit.getOrElse(0) >= 0)
    require(maybeRuntimeLimitInSeconds.getOrElse(0) >= 0)
}
