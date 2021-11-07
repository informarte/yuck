package yuck.annealing

import yuck.core._
import yuck.util.arm.ManagedResource
import yuck.util.logging.LazyLogger

/**
 * A managed resource for logging solver events.
 *
 * @author Michael Marte
 */
class StandardAnnealingMonitor(logger: LazyLogger) extends AnnealingMonitor with ManagedResource {

    private trait ThreadState
    private case object ThreadIsIdle extends ThreadState
    private case object SolverIsRunnable extends ThreadState
    private case object SolverIsRunning extends ThreadState

    private var solverState = new ThreadLocal[ThreadState] {
        override def initialValue = ThreadIsIdle
    }
    protected var costsOfBestProposal: Costs = null

    override def open() = {
    }

    override def close() = {
    }

    override def onSolverLaunched(result: AnnealingResult) = {
        require(solverState.get == ThreadIsIdle)
        solverState.set(SolverIsRunnable)
        logger.log("Launched solver")
        logger.log("Initial proposal has quality %s".format(result.costsOfBestProposal))
        solverState.set(SolverIsRunning)
    }

    override def onSolverSuspended(result: AnnealingResult) = {
        require(solverState.get == SolverIsRunning)
        solverState.set(SolverIsRunnable)
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
        require(solverState.get == SolverIsRunnable)
        if (result.roundLogs.isEmpty) {
            logger.log("Resumed solver that was suspended before search")
        } else {
            logger.log("Resumed solver in round %d".format(result.roundLogs.size))
        }
        solverState.set(SolverIsRunning)
    }

    override def onSolverFinished(result: AnnealingResult) = {
        require(solverState.get == SolverIsRunning)
        solverState.set(ThreadIsIdle)
        logger.criticalSection {
            if (result.roundLogs.isEmpty) {
                logger.log("Solver finished with proposal of quality %s".format(result.costsOfBestProposal))
            } else {
                logger.log(
                    "Solver finished with proposal of quality %s in round %d".format(
                        result.costsOfBestProposal, result.roundLogs.size))
            }
            logStatistics(result)
        }
    }

    override def onNextRound(result: AnnealingResult) = {
        logger.loggg("%s".format(result.roundLogs.last.toString))
    }

    override def onBetterProposal(result: AnnealingResult) = {
        synchronized {
            if (costsOfBestProposal.eq(null) ||
                result.objective.isLowerThan(result.costsOfBestProposal, costsOfBestProposal))
            {
                costsOfBestProposal = result.costsOfBestProposal
                logger.log(
                    "Improved global proposal quality to %s in round %d".format(
                        result.costsOfBestProposal, result.roundLogs.size))
            } else {
                logger.logg(
                    "Improved local proposal quality to %s in round %d".format(
                        result.costsOfBestProposal, result.roundLogs.size))
            }
            if (result.isGoodEnough) {
                logger.log("Objective achieved")
            }
        }
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

    override def onObjectiveTightened(x: AnyVariable) = {
        logger.logg("Reduced domain of objective variable %s to %s".format(x, x.domain))
    }

    private def logStatistics(result: AnnealingResult): Unit = {
        logger.withLogScope("Solver statistics".format(result.solverName)) {
            logger.log("Number of rounds: %d".format(result.roundLogs.size))
            if (! result.roundLogs.isEmpty) {
                logger.log("Moves per second: %f".format(result.movesPerSecond))
                logger.log("Consultations per second: %f".format(result.consultationsPerSecond))
                logger.log("Consultations per move: %f".format(result.consultationsPerMove))
                logger.log("Commitments per second: %f".format(result.commitmentsPerSecond))
                logger.log("Commitments per move: %f".format(result.commitmentsPerMove))
            }
        }
    }

}
