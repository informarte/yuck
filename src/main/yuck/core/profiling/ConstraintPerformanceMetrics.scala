package yuck.core.profiling

import java.time.Duration

final class ConstraintPerformanceMetrics(
    val numberOfConsultations: Long,
    val consultationEffort: Duration, // without profiling overhead
    val numberOfCommitments: Long,
    val commitmentEffort: Duration // without profiling overhead
)
