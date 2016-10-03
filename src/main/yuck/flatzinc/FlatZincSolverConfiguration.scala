package yuck.flatzinc

import yuck.annealing.{DEFAULT_MOVE_SIZE_DISTRIBUTION, DEFAULT_PROBABILITY_OF_FAIR_CHOICE_IN_PERCENT}
import yuck.core.{DEFAULT_SEED, DEFAULT_NUMBER_OF_RESTARTS, Distribution}

/**
 * @author Michael Marte
 *
 */
case class FlatZincSolverConfiguration(
    val seed: Int = DEFAULT_SEED,
    val numberOfRestarts: Int = DEFAULT_NUMBER_OF_RESTARTS,
    val numberOfVirtualCores: Int = Runtime.getRuntime.availableProcessors,
    val maybeRoundLimit: Option[Int] = None,
    val maybeRuntimeLimitInSeconds: Option[Int] = Some(300),
    val maybeOptimum: Option[Int] = None,
    val maybeQualityTolerance: Option[Int] = None,
    val stopOnFirstSolution: Boolean = false,
    val checkConstraintPropagation: Boolean = false,
    val moveSizeDistribution: Distribution = DEFAULT_MOVE_SIZE_DISTRIBUTION,
    val probabilityOfFairChoiceInPercent: Int = DEFAULT_PROBABILITY_OF_FAIR_CHOICE_IN_PERCENT)
{}
