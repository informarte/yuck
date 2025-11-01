package yuck.core.profiling

import java.time.Duration

import scala.collection.*

import yuck.core.{Constraint, Goal}

final class SpacePerformanceMetrics(
    val numberOfConsultations: Long,
    val consultationEffort: Duration, // without profiling overhead
    val numberOfCommitments: Long,
    val commitmentEffort: Duration, // without profiling overhead
    val performanceMetricsByConstraint:
        immutable.Map[Class[? <: Constraint], ConstraintPerformanceMetrics],
    val maybePerformanceMetricsByGoalAndConstraint:
        Option[immutable.Map[Goal, immutable.Map[Class[? <: Constraint], ConstraintPerformanceMetrics]]]
)
