package yuck.flatzinc

import yuck.core.{DEFAULT_RESTART_LIMIT, DEFAULT_SEED, Distribution, DistributionFactory, Probability}

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
    val seed: Long = DEFAULT_SEED,
    val restartLimit: Int = DEFAULT_RESTART_LIMIT,
    val numberOfVirtualCores: Int = Runtime.getRuntime.availableProcessors,
    val maybeRoundLimit: Option[Int] = None,
    val maybeRuntimeLimitInSeconds: Option[Int] = None,
    val maybeTargetObjectiveValue: Option[Int] = None,
    val maybeQualityTolerance: Option[Int] = None,
    val stopOnFirstSolution: Boolean = false,
    val useProgressiveTightening: Boolean = true,
    val preferImplicitSolvingOverDomainPruning: Boolean = false,
    val checkConstraintPropagation: Boolean = false,
    val moveSizeDistribution: Distribution = DistributionFactory.createDistribution(1, List(90, 10)),
    val level0Configuration: FlatZincLevelConfiguration = FlatZincLevelConfiguration(true, Some(Probability.from(3))),
    val level1Configuration: FlatZincLevelConfiguration = FlatZincLevelConfiguration(true, Some(Probability.from(13))))
{
    require(restartLimit >= 0)
    require(numberOfVirtualCores > 0)
    require(maybeRoundLimit.getOrElse(0) >= 0)
    require(maybeRuntimeLimitInSeconds.getOrElse(0) >= 0)
}
