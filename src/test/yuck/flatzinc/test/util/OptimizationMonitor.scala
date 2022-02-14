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

    case class QualityImprovement(runtimeInMillis: Long, quality: NumericalValue[_])

    private var timeStampInMillis: Long = 0
    private var maybeSolvingTimeInMillis: Option[Long] = None
    private var maybeRuntimeToBestSolutionInMillis: Option[Long] = None
    private var runtimeInMillis: Long = 0
    private var maybeTrackArea: Option[Boolean] = None
    private var maybePreviousQuality: Option[NumericalValue[_]] = None
    private var area: Double = 0.0
    private var qualityStepFunction = new mutable.ArrayBuffer[QualityImprovement]

    private def quality: NumericalValue[_] =
        costsOfBestProposal.asInstanceOf[PolymorphicListValue].value(1).asInstanceOf[NumericalValue[_]]

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

    override def open() = {
        super.open()
        timeStampInMillis = System.currentTimeMillis
    }

    override def close() = {
        super.close()
        val now = System.currentTimeMillis
        if (maybeTrackArea.getOrElse(false)) {
            area += maybePreviousQuality.getOrElse(quality).toDouble * ((now - timeStampInMillis) / 1000.0)
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
                val now = System.currentTimeMillis
                runtimeInMillis += now - timeStampInMillis
                if (! maybeSolvingTimeInMillis.isDefined) {
                    maybeSolvingTimeInMillis = Some(runtimeInMillis)
                }
                maybeRuntimeToBestSolutionInMillis = Some(runtimeInMillis)
                val problemHasNumericalObjective =
                    costsOfBestProposal match {
                        case value: PolymorphicListValue =>
                            value.value.size == 2 && value.value(1).isInstanceOf[NumericalValue[_]]
                        case _ => false
                    }
                if (problemHasNumericalObjective) {
                    val quality = this.quality
                    if (qualityStepFunction.isEmpty || qualityStepFunction.last.quality != quality) {
                        if (maybeTrackArea.isEmpty) {
                            maybeTrackArea = Some(true)
                        }
                        if (maybeTrackArea.get) {
                            if (quality.toDouble < 0) {
                                maybeTrackArea = Some(false)
                            } else {
                                area += maybePreviousQuality.getOrElse(quality).toDouble * ((now - timeStampInMillis) / 1000.0)
                                maybePreviousQuality = Some(quality)
                            }
                        }
                        qualityStepFunction += QualityImprovement(runtimeInMillis, quality)
                    }
                }
                timeStampInMillis = now
            }
        }
    }

    // Runtime from opening this resource until the first solution was found.
    def maybeSolvingTimeInSeconds: Option[Double] = maybeSolvingTimeInMillis.map(_ / 1000.0)

    // Runtime from opening this resource until the best solution was found.
    def maybeRuntimeToBestSolutionInSeconds: Option[Double] = maybeRuntimeToBestSolutionInMillis.map(_ / 1000.0)

    // Runtime from opening this resource until closing it.
    def runtimeInSeconds: Double = runtimeInMillis / 1000.0

    // Integral of the quality step function over the runtime horizon.
    // Only available when no negative objective values were encountered during optimization.
    def maybeArea: Option[Double] = if (maybeTrackArea.getOrElse(false)) Some(area) else None

    def maybeQualityStepFunction: Option[Seq[QualityImprovement]] =
        if (qualityStepFunction.isEmpty) None else Some(qualityStepFunction.toSeq)

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
