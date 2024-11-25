package yuck.annealing

import scala.collection.mutable
import yuck.core.*
import yuck.flatzinc.compiler.FlatZincCompilerResult
import yuck.util.DescriptiveStatistics.median
import yuck.util.logging.LazyLogger

/**
 * A monitor for collecting solver statistics.
 *
 * Assumes that the solver either terminates by itself or gets suspended due
 * to a timeout.
 *
 * Does not support resumption.
 *
 * @author Michael Marte
 */
final class AnnealingStatisticsCollector(logger: LazyLogger) extends AnnealingMonitor {

    case class QualityImprovement(runtimeInMillis: Long, quality: NumericalValue[?])

    private var timeStampInMillis: Long = 0
    private var maybeRuntimeToFirstSolutionInMillis: Option[Long] = None
    private var maybeRuntimeToBestSolutionInMillis: Option[Long] = None
    private var runtimeInMillis: Long = 0

    private var costsOfBestProposal: Costs = null

    private enum AreaTrackingState {
        case AreaTrackingNotStarted, TrackingArea, AreaTrackingAborted, AreaTrackingFinished
    }
    import AreaTrackingState.*
    private var areaTrackingState = AreaTrackingNotStarted
    private var area: Double = 0.0
    private val qualityStepFunction = new mutable.ArrayBuffer[QualityImprovement]

    private def maybePreviousQuality: Option[NumericalValue[?]] =
        if qualityStepFunction.isEmpty then None else Some(qualityStepFunction.last.quality)

    private def currentQuality: NumericalValue[?] =
        costsOfBestProposal.asInstanceOf[PolymorphicListValue].value(1).asInstanceOf[NumericalValue[?]]

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
        timeStampInMillis = System.currentTimeMillis
    }

    override def close() = {
        val now = System.currentTimeMillis
        if (areaTrackingState == TrackingArea) {
            area += maybePreviousQuality.get.toDouble * ((now - timeStampInMillis) / 1000.0)
            logger.logg("Area updated to %.2f".format(area))
            areaTrackingState = AreaTrackingFinished
            logger.logg("Area tracking finished")
        }
        runtimeInMillis += now - timeStampInMillis
    }

    override def onSolverSuspended(result: AnnealingResult) = {
        // We assume that the solver timed out and that it will never be resumed.
        captureSolverStatistics(result)
    }

    override def onSolverResumed(result: AnnealingResult) = {
        ???
    }

    override def onSolverFinished(result: AnnealingResult) = {
        captureSolverStatistics(result)
    }

    override def onBetterProposal(result: AnnealingResult) = {
        synchronized {
            if (result.isSolution) {
                if (costsOfBestProposal.eq(null) ||
                    result.objective.isLowerThan(result.costsOfBestProposal, costsOfBestProposal))
                {
                    costsOfBestProposal = result.costsOfBestProposal
                    keepRecords(result)
                }
            }
        }
    }

    private def keepRecords(result: Result): Unit = {
        val now = System.currentTimeMillis
        runtimeInMillis += now - timeStampInMillis
        if (maybeRuntimeToFirstSolutionInMillis.isEmpty) {
            maybeRuntimeToFirstSolutionInMillis = Some(runtimeInMillis)
        }
        maybeRuntimeToBestSolutionInMillis = Some(runtimeInMillis)
        val compilerResult = result.maybeUserData.get.asInstanceOf[FlatZincCompilerResult]
        val maybeOptimizationMode = compilerResult.objective match {
            case hierarchicalObjective: HierarchicalObjective => hierarchicalObjective.primitiveObjectives(1) match {
                case numericalObjective: NumericalObjective[_] => Some(numericalObjective.optimizationMode)
                case _ => None
            }
            case _ => None
        }
        if (maybeOptimizationMode.isDefined) {
            if (areaTrackingState == AreaTrackingNotStarted) {
                areaTrackingState = TrackingArea
                logger.logg("Area tracking started")
            }
            if (areaTrackingState == TrackingArea) {
                if (currentQuality.toDouble < 0) {
                    areaTrackingState = AreaTrackingAborted
                    logger.logg("Area tracking aborted due to negative objective value")
                } else {
                    maybeOptimizationMode.get match {
                        case OptimizationMode.Min =>
                            area += maybePreviousQuality.getOrElse(currentQuality).toDouble * ((now - timeStampInMillis) / 1000.0)
                        case OptimizationMode.Max =>
                            if (maybePreviousQuality.isDefined) {
                                area += maybePreviousQuality.get.toDouble * ((now - timeStampInMillis) / 1000.0)
                            }
                    }
                    logger.logg("Area updated to %.2f".format(area))
                }
            }
            qualityStepFunction += QualityImprovement(runtimeInMillis, currentQuality)
        }
        timeStampInMillis = now
    }

    // Runtime from opening this resource until the first solution was found.
    def maybeRuntimeToFirstSolutionInSeconds: Option[Double] = maybeRuntimeToFirstSolutionInMillis.map(_ / 1000.0)

    // Runtime from opening this resource until the best solution was found.
    def maybeRuntimeToBestSolutionInSeconds: Option[Double] = maybeRuntimeToBestSolutionInMillis.map(_ / 1000.0)

    // Runtime from opening this resource until closing it.
    def runtimeInSeconds: Double = runtimeInMillis / 1000.0

    // Integral of the quality step function over the runtime horizon.
    // Only available when no negative objective values were encountered during optimization.
    def maybeArea: Option[Double] = if areaTrackingState == AreaTrackingFinished then Some(area) else None

    // Quality step function over the runtime horizon.
    // Only available when no negative objective values were encountered during optimization.
    def maybeQualityStepFunction: Option[Seq[QualityImprovement]] =
        if areaTrackingState == AreaTrackingFinished then Some(qualityStepFunction.toSeq) else None

    // Returns true iff search was required to achieve the objective.
    def wasSearchRequired: Boolean = ! solverStatistics.isEmpty

    // Do not use the following methods when there was no search!
    def numberOfRestarts: Int = solverStatistics.size - 1
    def movesPerSecond: Double = solverStatistics.map(_.movesPerSecond).median
    def consultationsPerSecond: Double = solverStatistics.map(_.consultationsPerSecond).median
    def consultationsPerMove: Double = solverStatistics.map(_.consultationsPerMove).median
    def commitmentsPerSecond: Double = solverStatistics.map(_.commitmentsPerSecond).median
    def commitmentsPerMove: Double = solverStatistics.map(_.commitmentsPerMove).median

}
