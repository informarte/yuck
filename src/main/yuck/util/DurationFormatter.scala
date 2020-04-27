package yuck.util

import java.time.Duration

/**
 * @author Michael Marte
 *
 */
object DurationFormatter {

    def format(duration: Duration): String = {
        val nanos = duration.toNanos
        if (nanos > 1e9) "%f s".format(nanos / 1e9)
        else if (nanos > 1e6) "%f ms".format(nanos / 1e6)
        else if (nanos > 1e3) "%f \u00B5s".format(nanos / 1e3)
        else "%d ns".format(nanos)
    }

}
