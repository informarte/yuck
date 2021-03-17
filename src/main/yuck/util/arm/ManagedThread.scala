package yuck.util.arm

import yuck.util.logging.LazyLogger

/**
 * Manages a thread.
 *
 * @author Michael Marte
 */
final class ManagedThread(thread: Thread, logger: LazyLogger) extends ManagedResource {

    override def open() = {
        logger.logg("Starting %s".format(thread.getName))
        thread.start()
    }

    override def close() = {
        logger.logg("Asking %s to stop".format(thread.getName))
        thread.interrupt()
        thread.join()
        logger.logg("%s terminated".format(thread.getName))
    }

}
