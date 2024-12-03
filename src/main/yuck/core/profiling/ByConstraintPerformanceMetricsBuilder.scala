package yuck.core.profiling

import scala.collection.*

import yuck.core.{Constraint, Space}

/**
 * Collects performance metrics per type of constraint.
 *
 * Since the Constraint interface has only a small number of implementations,
 * the collection process does hopefully not interfere too much with the workload.
 *
 * @author Michael Marte
 */
final class ByConstraintPerformanceMetricsBuilder
    (val space: Space)
    extends SpacePerformanceMetricsBuilder[Class[? <: Constraint]]
{

    override protected def keyOf(constraint: Constraint) = constraint.getClass

    override protected val constraintPerformanceMetricsBuilders =
        new mutable.AnyRefMap[Class[? <: Constraint], ConstraintPerformanceMetricsBuilder]

    override protected lazy val performanceMetricsByConstraint =
        constraintPerformanceMetricsBuilders.view.mapValues(_.build()).toMap

    override def build() = {

        new SpacePerformanceMetrics(
            numberOfConsultations, adjustedConsultationEffort,
            numberOfCommitments, adjustedCommitmentEffort,
            performanceMetricsByConstraint, None)

    }

}
