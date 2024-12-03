package yuck.core.profiling

import java.time.Duration

import scala.collection.{immutable, mutable}

import yuck.core.Constraint
import yuck.util.arm.StopWatch.{NumberOfWarmUpIterations, averageNanoTimeCallDurationInNanos}

abstract class SpacePerformanceMetricsBuilder[K]
    extends PerformanceMetricsBuilder[SpacePerformanceMetrics, SpacePerformanceMetricsBuilder[K]]
{

    protected def keyOf(constraint: Constraint): K

    protected val constraintPerformanceMetricsBuilders: mutable.Map[K, ConstraintPerformanceMetricsBuilder]

    final def consulted(constraint: Constraint, nanos: Long): SpacePerformanceMetricsBuilder[K] = {
        constraintPerformanceMetricsBuilders
            .getOrElseUpdate(keyOf(constraint), new ConstraintPerformanceMetricsBuilder)
            .consulted(nanos)
        this
    }

    final def committed(constraint: Constraint, nanos: Long): SpacePerformanceMetricsBuilder[K] = {
        constraintPerformanceMetricsBuilders
            .getOrElseUpdate(keyOf(constraint), new ConstraintPerformanceMetricsBuilder)
            .committed(nanos)
        this
    }

    protected lazy val performanceMetricsByConstraint:
        immutable.Map[Class[? <: Constraint], ConstraintPerformanceMetrics]

    private final lazy val averageDurationOfConstraintPerformanceMetricsAccessInNanos: Double = {

        if (constraintPerformanceMetricsBuilders.isEmpty) {
            0.0
        } else {

            // trigger JIT compilation of exerciseConstraintPerformanceMetricsAccess
            exerciseConstraintPerformanceMetricsAccess(NumberOfWarmUpIterations)

            // compute average access time
            val startTimeInNanos = System.nanoTime
            exerciseConstraintPerformanceMetricsAccess(NumberOfWarmUpIterations)
            val endTimeInNanos = System.nanoTime
            (endTimeInNanos - startTimeInNanos).toDouble / NumberOfWarmUpIterations
        }

    }

    private def exerciseConstraintPerformanceMetricsAccess(n: Int): Unit = {
        var i = 0
        var keysIt = constraintPerformanceMetricsBuilders.keys.iterator
        while (i < n) {
            i += 1
            constraintPerformanceMetricsBuilders
                .getOrElseUpdate(keysIt.next(), new ConstraintPerformanceMetricsBuilder)
            if (!keysIt.hasNext) {
                keysIt = constraintPerformanceMetricsBuilders.keys.iterator
            }
        }
    }

    protected final lazy val adjustedConsultationEffort: Duration =
        consultationEffort
            .toImmutableDuration
            .minusNanos(
                (performanceMetricsByConstraint.values.map(_.numberOfConsultations).sum * 2 *
                    (averageNanoTimeCallDurationInNanos +
                        averageDurationOfConstraintPerformanceMetricsAccessInNanos) + 0.5).toLong)
            .minusNanos((numberOfConsultations * averageNanoTimeCallDurationInNanos + 0.5).toLong)

    protected final lazy val adjustedCommitmentEffort: Duration =
        commitmentEffort
            .toImmutableDuration
            .minusNanos(
                (performanceMetricsByConstraint.values.map(_.numberOfCommitments).sum * 2 *
                    (averageNanoTimeCallDurationInNanos +
                        averageDurationOfConstraintPerformanceMetricsAccessInNanos) + 0.5).toLong)
            .minusNanos((numberOfCommitments * averageNanoTimeCallDurationInNanos + 0.5).toLong)

}
