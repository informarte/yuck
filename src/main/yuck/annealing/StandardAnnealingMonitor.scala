package yuck.annealing

import scala.collection._

import yuck.core._
import yuck.util.arm.ManagedResource
import yuck.util.logging.LazyLogger

/**
 * A managed resource for logging solver events.
 *
 * @author Michael Marte
 */
class StandardAnnealingMonitor(
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
    private var costsOfBestProposal: Costs = null

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
                captureSolverStatistics(result)
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
            captureSolverStatistics(result)
        }
    }

    override def onNextRound(result: AnnealingResult) {
        logger.loggg("%s".format(result.roundLogs.last.toString))
    }

    override def onBetterProposal(result: AnnealingResult) {
        synchronized {
            if (costsOfBestProposal == null ||
                result.objective.isLowerThan(result.costsOfBestProposal, costsOfBestProposal))
            {
                costsOfBestProposal = result.costsOfBestProposal
                logger.log(
                    "Improved global proposal quality to %s in round %d".format(
                        costsOfBestProposal, result.roundLogs.size - 1))
            } else {
                logger.logg(
                    "Improved local proposal quality to %s in round %d".format(
                        result.costsOfBestProposal, result.roundLogs.size - 1))
            }
            if (result.isGoodEnough) {
                logger.log("Objective achieved")
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

    override def onObjectiveTightened(x: AnyVariable) {
        logger.logg("Reduced domain of objective variable %s to %s".format(x, x.domain))
    }

    private def logStatistics(result: AnnealingResult) {
        logger.withLogScope("Solver statistics".format(result.solverName)) {
            logger.log("Number of rounds: %d".format(result.roundLogs.size))
            if (result.roundLogs.size > 0) {
                logger.log("Moves per second: %f".format(result.movesPerSecond))
                logger.log("Consultations per second: %f".format(result.consultationsPerSecond))
                logger.log("Consultations per move: %f".format(result.consultationsPerMove))
                logger.log("Commitments per second: %f".format(result.commitmentsPerSecond))
                logger.log("Commitments per move: %f".format(result.commitmentsPerMove))
            }
        }
    }

    private class SolverStatistics(
        val runtimeInSeconds: Double, val movesPerSecond: Double,
        val consultationsPerSecond: Double, val consultationsPerMove: Double,
        val commitmentsPerSecond: Double, val commitmentsPerMove: Double)

    private val solverStatistics = new mutable.ArrayBuffer[SolverStatistics]

    private def captureSolverStatistics(result: AnnealingResult) {
        if (! result.roundLogs.isEmpty) {
            solverStatistics +=
                new SolverStatistics(
                    result.runtimeInSeconds, result.movesPerSecond,
                    result.consultationsPerSecond, result.consultationsPerMove,
                    result.commitmentsPerSecond, result.commitmentsPerMove)
        }
    }

    def wasSearchRequired: Boolean = ! solverStatistics.isEmpty
    def numberOfRestarts: Int = scala.math.max(0, solverStatistics.size - 1)
    def runtimeInSeconds: Double = solverStatistics.iterator.map(_.runtimeInSeconds).sum
    def movesPerSecond: Double = solverStatistics.iterator.map(_.movesPerSecond).sum / solverStatistics.size
    def consultationsPerSecond: Double = solverStatistics.iterator.map(_.consultationsPerSecond).sum / solverStatistics.size
    def consultationsPerMove: Double = solverStatistics.iterator.map(_.consultationsPerMove).sum / solverStatistics.size
    def commitmentsPerSecond: Double = solverStatistics.iterator.map(_.commitmentsPerSecond).sum / solverStatistics.size
    def commitmentsPerMove: Double = solverStatistics.iterator.map(_.commitmentsPerMove).sum / solverStatistics.size

}
