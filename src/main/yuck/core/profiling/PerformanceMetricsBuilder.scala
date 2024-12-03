package yuck.core.profiling

abstract class PerformanceMetricsBuilder
    [PerformanceMetrics, Impl <: PerformanceMetricsBuilder[PerformanceMetrics, Impl]]
{

    protected var numberOfConsultations = 0L
    protected val consultationEffort = MutableDuration.of(0, 0)
    protected var numberOfCommitments = 0L
    protected val commitmentEffort = MutableDuration.of(0, 0)

    def consulted(nanos: Long): Impl = {
        numberOfConsultations += 1
        consultationEffort.plusNanos(nanos)
        this.asInstanceOf[Impl]
    }

    def committed(nanos: Long): Impl = {
        numberOfCommitments += 1
        commitmentEffort.plusNanos(nanos)
        this.asInstanceOf[Impl]
    }

    /** Aggregates the results of profiling. */
    def build(): PerformanceMetrics

}
