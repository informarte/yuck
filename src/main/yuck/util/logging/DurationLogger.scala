package yuck.util.logging

import yuck.util.arm.ManagedResource

/**
 * A managed resource that, upon closing, logs the time passed since opening.
 *
 * @author Michael Marte
 */
final class DurationLogger(logger: LazyLogger, operationName: String) extends ManagedResource {

    private var startTime = 0L

    override def open = {
        startTime = System.currentTimeMillis
        logger.log("%s".format(operationName))
    }

    override def close = {
        val endTime = System.currentTimeMillis
        logger.log("%s took %f seconds".format(operationName, (endTime - startTime) / 1000.0))
    }

}
