package yuck.util.logging

import java.util.logging.{Logger, Handler}

import yuck.util.arm.ManagedResource

/**
 * A managed resource that adds the given handler to the given logger upon opening and
 * removes it upon closing.
 *
 * @author Michael Marte
 */
final class ManagedLogHandler(logger: Logger, handler: Handler) extends ManagedResource {

    override def open() = {
        logger.addHandler(handler)
    }

    override def close() = {
        handler.close()
        logger.removeHandler(handler)
    }

}
