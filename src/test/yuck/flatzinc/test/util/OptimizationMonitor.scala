package yuck.flatzinc.test.util

import scala.collection.mutable

import yuck.annealing.{AnnealingResult, StandardAnnealingMonitor}
import yuck.core.{NumericalValue, PolymorphicListValue}
import yuck.util.logging.LazyLogger

/**
 * Collects statistics about the optimization process for testing purposes.
 *
 * Assumes that the solver either terminates by itself or gets suspended due
 * to a timeout.
 *
 * Does not support resumption.
 *
 * @author Michael Marte
 */
class OptimizationMonitor(logger: LazyLogger) extends StandardAnnealingMonitor(logger) {

    private var timeStampInMillis: Long = 0
    private var runtimeInMillis: Long = 0
    private var maybeTrackArea: Option[Boolean] = None
    private var maybePreviousQuality: Option[Double] = None
    private var area: Double = 0.0

    private def quality: Double =
        costsOfBestProposal.asInstanceOf[PolymorphicListValue].value(1).asInstanceOf[NumericalValue[_]].toDouble

    private class SolverStatistics(
        val runtimeInSeconds: Double, val movesPerSecond: Double,
        val consultationsPerSecond: Double, val consultationsPerMove: Double,
        val commitmentsPerSecond: Double, val commitmentsPerMove: Double)

    private val solverStatistics = new mutable.ArrayBuffer[SolverStatistics]

    private def captureSolverStatistics(result: AnnealingResult): Unit = {
        if (! result.roundLogs.isEmpty) {
            synchronized {
                solverStatistics +=
                    new SolverStatistics(
                        result.runtimeInSeconds, result.movesPerSecond,
                        result.consultationsPerSecond, result.consultationsPerMove,
                        result.commitmentsPerSecond, result.commitmentsPerMove)
            }
        }
    }

    override def open = {
        super.open
        timeStampInMillis = System.currentTimeMillis
    }

    override def close = {
        super.close
        val now = System.currentTimeMillis
        if (maybeTrackArea.getOrElse(false)) {
            area += maybePreviousQuality.getOrElse(quality) * ((now - timeStampInMillis) / 1000.0)
        }
        runtimeInMillis += now - timeStampInMillis
    }

    override def onSolverSuspended(result: AnnealingResult) = {
        super.onSolverSuspended(result)
        // We assume that the solver timed out and that it will never be resumed.
        captureSolverStatistics(result)
    }

    override def onSolverResumed(result: AnnealingResult) = {
        ???
    }

    override def onSolverFinished(result: AnnealingResult) = {
        super.onSolverFinished(result)
        captureSolverStatistics(result)
    }

    override def onBetterProposal(result: AnnealingResult) = {
        synchronized {
            super.onBetterProposal(result)
            if (result.isSolution) {
                if (maybeTrackArea.isEmpty) {
                    maybeTrackArea = costsOfBestProposal match {
                        case value: PolymorphicListValue => Some(value.value.size == 2)
                        case _ => Some(false)
                    }
                }
                if (maybeTrackArea.get) {
                    val now = System.currentTimeMillis
                    runtimeInMillis += now - timeStampInMillis
                    if (quality < 0) {
                        maybeTrackArea = Some(false)
                    } else {
                        area += maybePreviousQuality.getOrElse(quality) * ((now - timeStampInMillis) / 1000.0)
                        maybePreviousQuality = Some(quality)
                    }
                    timeStampInMillis = now
                }
            }
        }
    }

    // Runtime from opening this resource until closing it.
    def runtimeInSeconds: Double = scala.math.max(1L, runtimeInMillis) / 1000.0

    // Integral of the quality step function over the runtime horizon.
    // Only available when no negative objective values were encountered during optimization.
    def maybeArea: Option[Double] = if (maybeTrackArea.getOrElse(false)) Some(area) else None

    // Returns true iff search was required to achieve the objective.
    def wasSearchRequired: Boolean = ! solverStatistics.isEmpty

    // Do not use the following methods when there was no search!
    def numberOfRestarts: Int = solverStatistics.size - 1
    def movesPerSecond: Double = solverStatistics.iterator.map(_.movesPerSecond).sum / solverStatistics.size
    def consultationsPerSecond: Double = solverStatistics.iterator.map(_.consultationsPerSecond).sum / solverStatistics.size
    def consultationsPerMove: Double = solverStatistics.iterator.map(_.consultationsPerMove).sum / solverStatistics.size
    def commitmentsPerSecond: Double = solverStatistics.iterator.map(_.commitmentsPerSecond).sum / solverStatistics.size
    def commitmentsPerMove: Double = solverStatistics.iterator.map(_.commitmentsPerMove).sum / solverStatistics.size

}
