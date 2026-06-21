package yuck.flatzinc.util

import scala.collection.mutable

import yuck.core.*
import yuck.flatzinc.compiler.FlatZincCompilerResult
import yuck.util.DescriptiveStatistics.geometricMean
import yuck.util.logging.LazyLogger

/**
 * A monitor for collecting solver metrics.
 *
 * Assumes that the solver either terminates by itself or gets suspended due to a timeout.
 *
 * Does not support resumption.
 */
final class LocalSearchMetricsCollector(logger: LazyLogger) extends LocalSearchMonitor {

    final case class ObjectiveImprovement(runtimeInMillis: Long, objectiveValue: NumericalValue[?])

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
    private val objectiveStepFunction = new mutable.ArrayBuffer[ObjectiveImprovement]

    private def maybePreviousObjectiveValue: Option[NumericalValue[?]] =
        if objectiveStepFunction.isEmpty then None else Some(objectiveStepFunction.last.objectiveValue)

    private def currentObjectiveValue: NumericalValue[?] =
        costsOfBestProposal.asInstanceOf[PolymorphicListValue].value(1).asInstanceOf[NumericalValue[?]]

    private class SolverMetrics(
        val runtimeInSeconds: Double, val movesPerSecond: Double,
        val consultationsPerSecond: Double, val consultationsPerMove: Double,
        val commitmentsPerSecond: Double, val commitmentsPerMove: Double,
        val numberOfPerturbations: Int)

    private val solverMetrics = new mutable.ArrayBuffer[SolverMetrics]

    private def captureSolverMetrics(result: LocalSearchResult): Unit = {
        if result.searchWasPerformed then {
            synchronized {
                solverMetrics +=
                    new SolverMetrics(
                        result.runtimeInSeconds, result.movesPerSecond,
                        result.consultationsPerSecond, result.consultationsPerMove,
                        result.commitmentsPerSecond, result.commitmentsPerMove,
                        result.numberOfPerturbations)
            }
        }
    }

    override def open() = {
        timeStampInMillis = System.currentTimeMillis
    }

    override def close() = {
        val now = System.currentTimeMillis
        if areaTrackingState == TrackingArea then {
            area += maybePreviousObjectiveValue.get.toDouble * ((now - timeStampInMillis) / 1000.0)
            logger.logg("Area updated to %.2f".format(area))
            areaTrackingState = AreaTrackingFinished
            logger.logg("Area tracking finished")
        }
        runtimeInMillis += now - timeStampInMillis
    }

    override def onSolverSuspended(result: LocalSearchResult) = {
        // We assume that the solver timed out and that it will never be resumed.
        captureSolverMetrics(result)
    }

    override def onSolverResumed(result: LocalSearchResult) = {
        ???
    }

    override def onSolverFinished(result: LocalSearchResult) = {
        captureSolverMetrics(result)
    }

    override def onBetterProposal(result: LocalSearchResult) = {
        if result.isSolution then {
            synchronized {
                if costsOfBestProposal.eq(null) ||
                    result.objective.isLowerThan(result.costsOfBestProposal, costsOfBestProposal) then
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
        if maybeRuntimeToFirstSolutionInMillis.isEmpty then {
            maybeRuntimeToFirstSolutionInMillis = Some(runtimeInMillis)
        }
        maybeRuntimeToBestSolutionInMillis = Some(runtimeInMillis)
        val compilerResult = result.maybeUserData.get.asInstanceOf[FlatZincCompilerResult]
        val maybeOptimizationMode = compilerResult.objective match {
            case hierarchicalObjective: HierarchicalObjective => hierarchicalObjective.primitiveObjectives(1) match {
                case numericalObjective: NumericalObjective[?, ?, ?] => Some(numericalObjective.optimizationMode)
                case _ => None
            }
            case _ => None
        }
        if maybeOptimizationMode.isDefined then {
            if areaTrackingState == AreaTrackingNotStarted then {
                areaTrackingState = TrackingArea
                logger.logg("Area tracking started")
            }
            if areaTrackingState == TrackingArea then {
                if currentObjectiveValue.toDouble < 0 then {
                    areaTrackingState = AreaTrackingAborted
                    logger.logg("Area tracking aborted due to negative objective value")
                } else {
                    maybeOptimizationMode.get match {
                        case OptimizationMode.Min =>
                            area += maybePreviousObjectiveValue.getOrElse(currentObjectiveValue).toDouble * ((now - timeStampInMillis) / 1000.0)
                        case OptimizationMode.Max =>
                            if maybePreviousObjectiveValue.isDefined then {
                                area += maybePreviousObjectiveValue.get.toDouble * ((now - timeStampInMillis) / 1000.0)
                            }
                    }
                    logger.logg("Area updated to %.2f".format(area))
                }
            }
            objectiveStepFunction += ObjectiveImprovement(runtimeInMillis, currentObjectiveValue)
        }
        timeStampInMillis = now
    }

    // Runtime from opening this resource until the first solution was found.
    def maybeRuntimeToFirstSolutionInSeconds: Option[Double] = maybeRuntimeToFirstSolutionInMillis.map(_ / 1000.0)

    // Runtime from opening this resource until the best solution was found.
    def maybeRuntimeToBestSolutionInSeconds: Option[Double] = maybeRuntimeToBestSolutionInMillis.map(_ / 1000.0)

    // Runtime from opening this resource until closing it.
    def runtimeInSeconds: Double = runtimeInMillis / 1000.0

    // Integral of the objective step function over the runtime horizon.
    // Only available when no negative objective values were encountered during optimization.
    def maybeArea: Option[Double] = if areaTrackingState == AreaTrackingFinished then Some(area) else None

    // Objective step function over the runtime horizon.
    // Only available when no negative objective values were encountered during optimization.
    def maybeObjectiveStepFunction: Option[Seq[ObjectiveImprovement]] =
        if areaTrackingState == AreaTrackingFinished then Some(objectiveStepFunction.toSeq) else None

    // Returns true iff search was required to achieve the objective.
    def wasSearchRequired: Boolean = ! solverMetrics.isEmpty

    // Do not use the following methods when there was no search!
    def movesPerSecond: Double = solverMetrics.map(_.movesPerSecond).geometricMean
    def consultationsPerSecond: Double = solverMetrics.map(_.consultationsPerSecond).geometricMean
    def consultationsPerMove: Double = solverMetrics.map(_.consultationsPerMove).geometricMean
    def commitmentsPerSecond: Double = solverMetrics.map(_.commitmentsPerSecond).geometricMean
    def commitmentsPerMove: Double = solverMetrics.map(_.commitmentsPerMove).geometricMean
    def numberOfPerturbations: Double = solverMetrics.map(_.numberOfPerturbations.toDouble).geometricMean

}
