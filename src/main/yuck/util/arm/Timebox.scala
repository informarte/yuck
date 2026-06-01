package yuck.util.arm

import java.time.Duration
import java.util.concurrent.atomic.AtomicLong

import yuck.util.DurationFormatter
import yuck.util.logging.LazyLogger

/**
 * Sends an interrupt signal after reaching the given runtime limit.
 *
 * Stops the watch on interruption and resumes it on resumption.
 */
final class Timebox
    (runtimeLimitInMillis: AtomicLong,
     sigint: SettableSigint,
     operationName: String,
     logger: LazyLogger)
    extends Runnable
{

    private var totalSleepTimeInMillis = 0L

    def isOutOfTime: Boolean = runtimeLimitInMillis.get - totalSleepTimeInMillis <= 0

    override def run() = {
        logger.log("Runtime limit is %s".format(DurationFormatter.format(Duration.ofMillis(runtimeLimitInMillis.get))))
        var interrupted = false
        while ! interrupted && totalSleepTimeInMillis < runtimeLimitInMillis.get do {
            val t0 = System.currentTimeMillis
            try {
                Thread.sleep(runtimeLimitInMillis.get - totalSleepTimeInMillis)
            }
            catch {
                case _: InterruptedException =>
                    logger.log("Interrupted")
                    interrupted = true
            }
            finally {
                val t1 = System.currentTimeMillis
                totalSleepTimeInMillis += t1 - t0
            }
        }
        if ! interrupted then {
            logger.log("Out of time, asking %s to stop".format(operationName))
            sigint.set()
        }
    }

}
