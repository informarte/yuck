package yuck.util.arm

import yuck.util.logging.LazyLogger

/**
 * Manages a thread.
 */
final class ManagedThread(thread: Thread, logger: LazyLogger) extends ManagedResource {

    override def open() = {
        logger.log("Starting thread %s".format(thread.getName))
        thread.start()
    }

    override def close() = {
        if thread.isAlive then {
            logger.log("Asking thread %s to terminate".format(thread.getName))
            thread.interrupt()
            thread.join()
            logger.log("Thread %s terminated".format(thread.getName))
        }
    }

}
