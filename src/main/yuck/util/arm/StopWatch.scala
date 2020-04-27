package yuck.util.arm

import java.time.Duration

/**
 * A managed resource for measuring runtime in nanoseconds.
 *
 * @author Michael Marte
 */
final class StopWatch extends ManagedResource {

    private var startTimeInNanos = 0L
    private var endTimeInNanos = 0L

    override def open = {
        startTimeInNanos = System.nanoTime
    }

    override def close = {
        endTimeInNanos = System.nanoTime
    }

    def duration: Duration = Duration.ofNanos(endTimeInNanos - startTimeInNanos)

}
