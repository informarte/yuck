package yuck.core.profiling

import yuck.util.arm.StopWatch.averageNanoTimeCallDurationInNanos

private[profiling] final class ConstraintPerformanceMetricsBuilder
    extends PerformanceMetricsBuilder[ConstraintPerformanceMetrics, ConstraintPerformanceMetricsBuilder]
{

    def add(other: ConstraintPerformanceMetricsBuilder): ConstraintPerformanceMetricsBuilder = {
        numberOfConsultations += other.numberOfConsultations
        consultationEffort.plus(other.consultationEffort)
        numberOfCommitments += other.numberOfCommitments
        commitmentEffort.plus(other.commitmentEffort)
        this
    }

    override def build(): ConstraintPerformanceMetrics = {
        val adjustedConsultationEffort =
            consultationEffort
                .toImmutableDuration
                .minusNanos((numberOfConsultations * averageNanoTimeCallDurationInNanos + 0.5).toLong)
        val adjustedCommitmentEffort =
            commitmentEffort
                .toImmutableDuration
                .minusNanos((numberOfCommitments * averageNanoTimeCallDurationInNanos + 0.5).toLong)
        new ConstraintPerformanceMetrics(
            numberOfConsultations, adjustedConsultationEffort,
            numberOfCommitments, adjustedCommitmentEffort)
    }

}
