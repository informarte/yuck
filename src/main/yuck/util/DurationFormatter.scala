package yuck.util

import java.time.Duration

object DurationFormatter {

    def format(duration: Duration): String = {
        val nanos = duration.toNanos
        if nanos > 1e9
        then "%f s".format(nanos / 1e9)
        else if nanos > 1e6
        then "%f ms".format(nanos / 1e6)
        else if nanos > 1e3
        then "%f \u00B5s".format(nanos / 1e3)
        else "%d ns".format(nanos)
    }

}
