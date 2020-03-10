package yuck.util.logging

import yuck.util.arm.ManagedResource

/**
 * A managed resource that, upon closing, logs the time passed since opening.
 *
 * @author Michael Marte
 */
final class DurationLogger(logger: LazyLogger, operationName: String) extends ManagedResource {

    private var startTimeInMillis = 0L

    override def open = {
        startTimeInMillis = System.currentTimeMillis
        logger.log("%s".format(operationName))
    }

    override def close = {
        val now = System.currentTimeMillis
        logger.log("%s took %f seconds".format(operationName, (now - startTimeInMillis) / 1000.0))
    }

}
