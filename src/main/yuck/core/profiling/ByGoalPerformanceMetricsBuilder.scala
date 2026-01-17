package yuck.core.profiling

import scala.collection.*

import yuck.core.profiling.*
import yuck.core.{Constraint, Goal, Space}

/**
 * Collects performance metrics per constraint and groups them by goal.
 *
 * For each goal, all constraints involved in the goal are considered, even
 * those which don't have a goal attribution or are involved in more than
 * one goal. (The latter are accordingly considered more than once.)
 *
 * Notice that the metrics are collected for each single constraint posted
 * to the given space, possibly resulting in a large map. Hence the collection
 * process might interfere with the workload.
 */
final class ByGoalPerformanceMetricsBuilder(val space: Space) extends SpacePerformanceMetricsBuilder[Constraint] {

    override protected def keyOf(constraint: Constraint) = constraint

    override protected val constraintPerformanceMetricsBuilders =
        new mutable.HashMap[Constraint, ConstraintPerformanceMetricsBuilder]

    override protected lazy val performanceMetricsByConstraint =
        aggregatedPerformanceMetricsByConstraint(constraintPerformanceMetricsBuilders.keySet)

    override def build(): SpacePerformanceMetrics = {

        val constraints = constraintPerformanceMetricsBuilders.keySet

        val constraintsByGoal: immutable.Map[Goal, mutable.Set[Constraint]] =
            constraints.view
                .flatMap(constraint => space.goals(constraint).view.map(goal => (goal, mutable.Set(constraint))))
                .groupMapReduce(_._1)(_._2)((lhs, rhs) => lhs ++ rhs)

        for goal <- constraintsByGoal.keys do {
            for constraint <- constraintsByGoal(goal).clone() do {
                for x <- constraint.inVariables do {
                    constraintsByGoal(goal) ++= space.involvedConstraints(x)
                }
            }
        }

        val constraintPerformanceMetricsByGoalAndConstraint:
            immutable.Map[Goal, immutable.Map[Class[? <: Constraint], ConstraintPerformanceMetrics]] =
            constraintsByGoal.view.mapValues(aggregatedPerformanceMetricsByConstraint).toMap

        new SpacePerformanceMetrics(
            numberOfConsultations, adjustedConsultationEffort,
            numberOfCommitments, adjustedCommitmentEffort,
            performanceMetricsByConstraint, Some(constraintPerformanceMetricsByGoalAndConstraint))

    }

    private def aggregatedPerformanceMetricsByConstraint
        (constraints: Iterable[Constraint]): immutable.Map[Class[? <: Constraint], ConstraintPerformanceMetrics] =
    {
        constraints
            .groupBy(_.getClass)
            .view
            .mapValues(
                _.foldLeft(new ConstraintPerformanceMetricsBuilder)((builder, constraint) =>
                        if constraintPerformanceMetricsBuilders.contains(constraint)
                        then builder.add(constraintPerformanceMetricsBuilders(constraint))
                        else builder)
                    .build())
            .toMap
    }

}
