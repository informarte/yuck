package yuck.annealing

import yuck.core._
import yuck.util.arm.ManagedResource
import yuck.util.logging.LazyLogger

/**
 * A managed resource for logging solver events.
 *
 * @author Michael Marte
 */
final class StandardAnnealingMonitor(
    logger: LazyLogger)
    extends AnnealingMonitor with ManagedResource
{

    private trait ThreadState
    private case object ThreadIsIdle extends ThreadState
    private case object SolverIsRunnable extends ThreadState
    private case object SolverIsRunning extends ThreadState

    private var solverState = new ThreadLocal[ThreadState] {
        override def initialValue = ThreadIsIdle
    }
    private var costsOfBestSolution: Costs = null

    override def open {
        if (solverState.get == SolverIsRunnable) {
        }
    }

    override def close {
        if (solverState.get == SolverIsRunning) {
        }
    }

    override def onSolverLaunched(result: AnnealingResult) {
        require(solverState.get == ThreadIsIdle)
        solverState.set(SolverIsRunnable)
        open
        logger.log("Launched solver")
        solverState.set(SolverIsRunning)
    }

    override def onSolverSuspended(result: AnnealingResult) {
        require(solverState.get == SolverIsRunning)
        close
        solverState.set(SolverIsRunnable)
        if (result.roundLogs.isEmpty) {
            logger.log("Suspended solver before first round")
        } else {
            logger.criticalSection {
                logger.log("Suspended solver in round %d".format(result.roundLogs.size - 1))
                logStatistics(result)
            }
        }
    }

    override def onSolverResumed(result: AnnealingResult) {
        require(solverState.get == SolverIsRunnable)
        if (result.roundLogs.isEmpty) {
            logger.log("Resumed solver that was suspended before first round")
        } else {
            logger.log("Resumed solver in round %d".format(result.roundLogs.size - 1))
        }
        solverState.set(SolverIsRunning)
        open
    }

    override def onSolverFinished(result: AnnealingResult) {
        require(solverState.get == SolverIsRunning)
        close
        solverState.set(ThreadIsIdle)
        logger.criticalSection {
            logger.log(
                "Solver finished with proposal of quality %s in round %d".format(
                    result.costsOfBestProposal, result.roundLogs.size - 1))
            logStatistics(result)
        }
    }

    override def onNextRound(result: AnnealingResult) {
        logger.loggg("%s".format(result.roundLogs.last.toString))
    }

    override def onBetterProposal(result: AnnealingResult) {
        synchronized {
            if (result.isSolution) {
                if (costsOfBestSolution == null ||
                    result.objective.isLowerThan(result.costsOfBestProposal, costsOfBestSolution))
                {
                    costsOfBestSolution = result.costsOfBestProposal
                    logger.log(
                        "Improved global solution quality to %s in round %d".format(
                            costsOfBestSolution, result.roundLogs.size - 1))
                } else {
                    logger.logg(
                        "Improved local solution quality to %s in round %d".format(
                            result.costsOfBestProposal, result.roundLogs.size - 1))
                }
            }
        }
    }

    override def onReheatingStarted(result: AnnealingResult) {
        val roundLog = result.roundLogs.last
        logger.logg(
            "Reheating started after round %d from uphill acceptance ratio %1.6f at temperature %3.6f".format(
                result.roundLogs.size - 1, roundLog.uphillAcceptanceRatio, roundLog.temperature))
    }

    override def onReheatingFinished(result: AnnealingResult) {
        val roundLog = result.roundLogs.last
        logger.logg(
            "Reheating finished after round %d with uphill acceptance ratio %1.6f at temperature %3.6f".format(
                result.roundLogs.size - 1, roundLog.uphillAcceptanceRatio, roundLog.temperature))
    }

    private def logStatistics(result: AnnealingResult) {
        logger.withLogScope("Solver statistics".format(result.solverName)) {
            logger.log("Number of rounds: %d".format(result.roundLogs.size))
            if (result.roundLogs.size > 0) {
                logger.log("Moves per second: %d".format(result.movesPerSecond))
                logger.log("Consultations per second: %d".format(result.consultationsPerSecond))
                logger.log("Consultations per move: %d".format(result.consultationsPerMove))
                logger.log("Commitments per second: %d".format(result.commitmentsPerSecond))
                logger.log("Commitments per move: %d".format(result.commitmentsPerMove))
            }
        }
    }

}
