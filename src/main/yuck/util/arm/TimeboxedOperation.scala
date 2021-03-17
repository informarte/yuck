package yuck.util.arm

import java.util.concurrent.Callable

import yuck.util.logging.{LazyLogger, TransientThreadRenaming}

/**
 * Interrupts the given operation after reaching the given runtime limit.
 *
 * Stops the watch on interruption and resumes it on resumption.
 *
 * @author Michael Marte
 */
final class TimeboxedOperation
    [Result]
    (operation: => Result,
     runtimeLimitInSeconds: Int,
     sigint: SettableSigint,
     operationName: String,
     logger: LazyLogger)
    extends Callable[Result]
{

    private var remainingRuntimeInMillis: Long = runtimeLimitInSeconds * 1000

    def isOutOfTime: Boolean = remainingRuntimeInMillis <= 0

    override def call() = {
        val watchdog = new Thread {
            override def run() = {
                val t0 = System.currentTimeMillis
                try {
                    if (remainingRuntimeInMillis > 0) {
                        Thread.sleep(remainingRuntimeInMillis)
                    }
                }
                catch {
                    case error: InterruptedException =>
                        logger.logg("Interrupted")
                }
                finally {
                    val t1 = System.currentTimeMillis
                    remainingRuntimeInMillis -= (t1 - t0)
                }
                if (remainingRuntimeInMillis <= 0) {
                    logger.log("Out of time, asking %s to stop".format(operationName))
                    sigint.set()
                }
            }
        }
        scoped(new TransientThreadRenaming(watchdog, "%s-watchdog".format(operationName))) {
            scoped(new ManagedThread(watchdog, logger)) {
                operation
            }
        }
    }

}
