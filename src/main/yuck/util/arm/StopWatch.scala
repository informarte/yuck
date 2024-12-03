package yuck.util.arm

import java.time.Duration

/**
 * A managed resource for measuring runtime in nanoseconds.
 *
 * See https://www.javaadvent.com/2019/12/measuring-time-from-java-to-kernel-and-back.html
 * for background on measuring time on Linux systems.
 *
 * @author Michael Marte
 */
final class StopWatch extends ManagedResource {

    private var startTimeInNanos = 0L
    private var endTimeInNanos = 0L

    override def open() = {
        startTimeInNanos = System.nanoTime
    }

    override def close() = {
        endTimeInNanos = System.nanoTime
    }

    def duration: Duration = Duration.ofNanos(endTimeInNanos - startTimeInNanos)

}

/**
 * @author Michael Marte
 *
 */
object StopWatch {

    // tested on OpenJDK 21
    val NumberOfWarmUpIterations = 1000_000

    lazy val averageNanoTimeCallDurationInNanos: Double = {

        // trigger JIT compilation of exerciseNanoTime
        exerciseNanoTime(NumberOfWarmUpIterations)

        // compute average runtime of System.nanoTime
        val startTimeInNanos = System.nanoTime
        exerciseNanoTime(NumberOfWarmUpIterations)
        val endTimeInNanos = System.nanoTime
        (endTimeInNanos - startTimeInNanos).toDouble / NumberOfWarmUpIterations

    }

    private def exerciseNanoTime(n: Int): Unit = {
        var i = 0
        while (i < n) {
            System.nanoTime
            i += 1
        }
    }

}
