package yuck.util.logging

import yuck.util.DurationFormatter
import yuck.util.arm.{ManagedResource, StopWatch}

/**
 * A managed resource that, upon closing, logs the time passed since opening.
 *
 * @author Michael Marte
 */
final class DurationLogger(logger: LazyLogger, operationName: String) extends ManagedResource {

    private val stopWatch = new StopWatch

    override def open = {
        logger.log("%s".format(operationName))
        stopWatch.open
    }

    override def close = {
        stopWatch.close
        logger.log("%s took %s".format(operationName, DurationFormatter.format(stopWatch.duration)))
    }

}
