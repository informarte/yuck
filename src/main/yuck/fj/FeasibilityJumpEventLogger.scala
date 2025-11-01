package yuck.fj

import yuck.core.AnyVariable
import yuck.util.logging.LazyLogger

/**
 * A monitor for logging solver events.
 *
 * @author Michael Marte
 */
final class FeasibilityJumpEventLogger(logger: LazyLogger) extends FeasibilityJumpMonitor {

    override def onSolverLaunched(result: FeasibilityJumpResult) = {
        logger.log("Launched solver")
        logger.log("Initial proposal has quality %s".format(result.costsOfBestProposal))
    }

    override def onSolverSuspended(result: FeasibilityJumpResult) = {
        if (result.searchWasPerformed) {
            logger.criticalSection {
                logger.log("Suspended solver")
                logStatistics(result)
            }
        } else {
            logger.log("Suspended solver before search")
        }
    }

    override def onSolverResumed(result: FeasibilityJumpResult) = {
        if (result.searchWasPerformed) {
            logger.log("Resumed solver")
        } else {
            logger.log("Resumed solver that was suspended before search")
        }
    }

    override def onSolverFinished(result: FeasibilityJumpResult) = {
        logger.criticalSection {
            logger.log("Solver finished with proposal of quality %s".format(result.costsOfBestProposal))
            if (result.searchWasPerformed) {
                logStatistics(result)
            }
        }
    }

    override def onBetterProposal(result: FeasibilityJumpResult) = {
        logger.logg("Improved local proposal quality to %s".format(result.costsOfBestProposal))
    }

    override def onObjectiveTightened(result: FeasibilityJumpResult, x: AnyVariable) = {
        logger.logg("Reduced domain of objective variable %s to %s".format(x, x.domain))
    }

    override def onNextRound(result: FeasibilityJumpResult) = {
        logger.logg("Round started")
    }

    override def onPerturbation(result: FeasibilityJumpResult) = {
        logger.logg("Perturbed assignment")
    }

    private def logStatistics(result: FeasibilityJumpResult): Unit = {
        logger.withLogScope("Solver statistics") {
            logger.log("Moves per second: %f".format(result.movesPerSecond))
            logger.log("Consultations per second: %f".format(result.consultationsPerSecond))
            logger.log("Consultations per move: %f".format(result.consultationsPerMove))
            logger.log("Commitments per second: %f".format(result.commitmentsPerSecond))
            logger.log("Commitments per move: %f".format(result.commitmentsPerMove))
            logger.log("Number of perturbations: %d".format(result.numberOfPerturbations))
        }
    }

}
