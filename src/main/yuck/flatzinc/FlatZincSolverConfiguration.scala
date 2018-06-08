package yuck.flatzinc

import yuck.annealing.{DEFAULT_MOVE_SIZE_DISTRIBUTION}
import yuck.core.{DEFAULT_SEED, DEFAULT_RESTART_LIMIT, Distribution, Probability}

/**
 * @author Michael Marte
 *
 */
case class FlatZincSolverConfiguration(
    val seed: Int = DEFAULT_SEED,
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
    val moveSizeDistribution: Distribution = DEFAULT_MOVE_SIZE_DISTRIBUTION,
    val maybeFairVariableChoiceRate: Option[Probability] = Some(Probability.from(3)),
    val focusOnConstraintViolations: Boolean = true,
    val guideOptimization: Boolean = false)
{
    require(restartLimit >= 0)
    require(numberOfVirtualCores > 0)
    require(maybeRoundLimit.getOrElse(0) >= 0)
    require(maybeRuntimeLimitInSeconds.getOrElse(0) >= 0)
}
