package yuck.test.util

import java.util.concurrent.{Executors, TimeUnit}

import org.junit.runners.model.RunnerScheduler

/**
 * Scheduler for running JUnit tests in parallel
 *
 * @author Michael Marte
 */
class ParallelTestScheduler extends RunnerScheduler {

    final private val service = Executors.newFixedThreadPool(DefaultNumberOfThreads)

    override def schedule(childStatement: Runnable): Unit = {
        service.submit(childStatement)
    }

    override def finished(): Unit = {
        try {
            service.shutdown()
            service.awaitTermination(Long.MaxValue, TimeUnit.NANOSECONDS)
        } catch {
            case e: InterruptedException =>
                e.printStackTrace(System.err)
        }
    }

}
