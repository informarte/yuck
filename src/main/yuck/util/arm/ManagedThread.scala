package yuck.util.arm

import yuck.util.logging.LazyLogger

/**
 * Manages a thread.
 *
 * @author Michael Marte
 */
final class ManagedThread(thread: Thread, logger: LazyLogger) extends ManagedResource {

    override def open() = {
        logger.logg("Starting thread %s".format(thread.getName))
        thread.start()
    }

    override def close() = {
        logger.logg("Asking thread %s to stop".format(thread.getName))
        thread.interrupt()
        thread.join()
        logger.logg("Thread %s terminated".format(thread.getName))
    }

}
