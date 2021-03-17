package yuck.util.arm

import java.util.concurrent.ExecutorService
import java.util.concurrent.TimeUnit

import yuck.util.logging.LazyLogger

/**
 * Manages a [[http://docs.oracle.com/javase/7/docs/api/java/util/concurrent/ExecutorService.html Java executor service]].
 *
 * @author Michael Marte
 */
final class ManagedExecutorService(executor: ExecutorService, logger: LazyLogger) extends ManagedResource {

    override def open() = {
        logger.logg("Using %s".format(executor))
    }

    override def close() = {
        logger.logg("Shutting down %s".format(executor))
        executor.shutdownNow()
        executor.awaitTermination(1, TimeUnit.MINUTES)
    }

}
