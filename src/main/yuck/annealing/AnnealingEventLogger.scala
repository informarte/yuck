package yuck.annealing

import yuck.core.AnyVariable
import yuck.util.logging.LazyLogger

/**
 * A monitor for logging solver events.
 *
 * @author Michael Marte
 */
final class AnnealingEventLogger(logger: LazyLogger) extends AnnealingMonitor {

    override def onSolverLaunched(result: AnnealingResult) = {
        logger.log("Launched solver")
        logger.log("Initial proposal has quality %s".format(result.costsOfBestProposal))
    }

    override def onSolverSuspended(result: AnnealingResult) = {
        if (result.roundLogs.isEmpty) {
            logger.log("Suspended solver before search")
        } else {
            logger.criticalSection {
                logger.log("Suspended solver in round %d".format(result.roundLogs.size))
                logStatistics(result)
            }
        }
    }

    override def onSolverResumed(result: AnnealingResult) = {
        if (result.roundLogs.isEmpty) {
            logger.log("Resumed solver that was suspended before search")
        } else {
            logger.log("Resumed solver in round %d".format(result.roundLogs.size))
        }
    }

    override def onSolverFinished(result: AnnealingResult) = {
        logger.criticalSection {
            if (result.roundLogs.isEmpty) {
                logger.log("Solver finished with proposal of quality %s".format(result.costsOfBestProposal))
            } else {
                logger.criticalSection {
                    logger.log(
                        "Solver finished with proposal of quality %s in round %d".format(
                            result.costsOfBestProposal, result.roundLogs.size))
                    logStatistics(result)
                }
            }
        }
    }

    override def onBetterProposal(result: AnnealingResult) = {
        logger.logg(
            "Improved local proposal quality to %s in round %d".format(
                result.costsOfBestProposal, result.roundLogs.size))
    }

    override def onNextRound(result: AnnealingResult) = {
        logger.loggg("%s".format(result.roundLogs.last.toString))
    }

    override def onReheatingStarted(result: AnnealingResult) = {
        val roundLog = result.roundLogs.last
        logger.logg(
            "Reheating started after round %d from uphill acceptance ratio %1.6f at temperature %3.6f".format(
                result.roundLogs.size, roundLog.uphillAcceptanceRatio, roundLog.temperature))
    }

    override def onReheatingFinished(result: AnnealingResult) = {
        val roundLog = result.roundLogs.last
        logger.logg(
            "Reheating finished after round %d with uphill acceptance ratio %1.6f at temperature %3.6f".format(
                result.roundLogs.size, roundLog.uphillAcceptanceRatio, roundLog.temperature))
    }

    override def onScheduleRestarted(result: AnnealingResult) = {
        val roundLog = result.roundLogs.last
        logger.logg("Schedule restarted after round %d".format(result.roundLogs.size))
    }

    override def onObjectiveTightened(result: AnnealingResult, x: AnyVariable) = {
        logger.logg("Reduced domain of objective variable %s to %s".format(x, x.domain))
    }

    private def logStatistics(result: AnnealingResult): Unit = {
        logger.withLogScope("Solver statistics") {
            val roundLogs = result.roundLogs
            logger.log("Number of rounds: %d".format(result.roundLogs.size))
            logger.log("Moves per second: %f".format(result.movesPerSecond))
            logger.log("Consultations per second: %f".format(result.consultationsPerSecond))
            logger.log("Consultations per move: %f".format(result.consultationsPerMove))
            logger.log("Commitments per second: %f".format(result.commitmentsPerSecond))
            logger.log("Commitments per move: %f".format(result.commitmentsPerMove))
            logger.log("Number of perturbations: %d".format(result.numberOfPerturbations))
        }
    }

}
