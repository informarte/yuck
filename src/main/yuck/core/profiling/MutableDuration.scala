package yuck.core.profiling

import java.time.Duration

private[profiling] final class MutableDuration private(private var _seconds: Long = 0L, private var _nanos: Int = 0) {

    import MutableDuration.*

    def seconds: Long = _seconds

    def nanos: Int = _nanos

    private def plus(secondsToAdd: Long, nanosToAdd: Long): MutableDuration = {
        require(secondsToAdd >= 0)
        require(nanosToAdd >= 0)
        _seconds = Math.addExact(Math.addExact(_seconds, secondsToAdd), nanosToAdd / NanosPerSecond)
        _nanos = _nanos + Math.toIntExact(nanosToAdd % NanosPerSecond)
        if (_nanos > NanosPerSecond) {
            _seconds = Math.incrementExact(_seconds)
            _nanos = _nanos % NanosPerSecond
        }
        this
    }

    def plus(other: MutableDuration): MutableDuration = {
        plus(other._seconds, other._nanos)
    }

    def plusNanos(nanos: Long): MutableDuration = {
        plus(0, nanos)
    }

    def toImmutableDuration: Duration = Duration.ofSeconds(_seconds, _nanos)

}

object MutableDuration {

    val NanosPerSecond = 1000_000_000

    def of(seconds: Long, nanoAdjustment: Long): MutableDuration =
        MutableDuration().plus(seconds, nanoAdjustment)

}
