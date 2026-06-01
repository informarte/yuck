package yuck.flatzinc.util

import java.time.Duration
import java.util.concurrent.atomic.AtomicLong
import java.util.concurrent.{CompletableFuture, CountDownLatch}

import org.openjdk.jol.info.GraphLayout

import yuck.core.{Result, Solver, SolverMonitor, min}
import yuck.flatzinc.FlatZincSolverConfiguration
import yuck.util.DurationFormatter
import yuck.util.arm.Sigint
import yuck.util.logging.LazyLogger

/**
 * Measures a solver's memory footprint when the maximum number of workers is
 * running concurrently, i.e. once min(cfg.numberOfSolvers, cfg.numberOfThreads)
 * workers have launched. Each running worker waits in onSolverLaunched until
 * the measurement completes, so that memory is sampled while all concurrently
 * allocated resources are still live.
 *
 * Designed to run in its own thread.
 *
 * Normal flow: The required number of workers launch and block. The memory
 * footprint of the solver registered via setSolver is measured and published via
 * maybeMemoryFootprintInBytes. The blocked workers are then resumed.
 *
 * Exceptional flow 1: No solver was registered via setSolver. The blocked
 * workers are resumed without a measurement.
 *
 * Exceptional flow 2: The monitor thread is interrupted before the required number of
 * workers was launched. The blocked workers are resumed without a measurement.
 *
 * By registering itself with sigint, the monitor thread addresses the following
 * deadlock scenarios:
 *
 * 1. A worker found a good-enough solution early, causing ParallelSolver to terminate
 *    before the required number of workers was launched.
 * 2. Some workers are waiting in onSolverLaunched for the measurement to complete while
 *    others will never launch because of timeout during compilation.
 */
final class MemoryFootprintMonitor
    (cfg: FlatZincSolverConfiguration,
     maybeMemoryFootprintInBytes: CompletableFuture[Option[Long]],
     maybeRuntimeLimitInMillis: Option[AtomicLong],
     logger: LazyLogger,
     sigint: Sigint)
    extends SolverMonitor
    with Runnable
{

    private val workersRemaining = new CountDownLatch(min(cfg.numberOfSolvers, cfg.numberOfThreads))
    private val memoryFootprintMeasured = new CountDownLatch(1)

    private var solver: Solver = null

    def setSolver(solver: Solver) = {
        this.solver = solver
    }

    override def onSolverLaunched(result: Result): Unit = {
        logger.log("Blocking until memory footprint was measured")
        workersRemaining.countDown()
        memoryFootprintMeasured.await()
        logger.log("Resuming")
    }

    override def run() = {
        sigint.registerListener(Thread.currentThread)
        try {
            measureMemoryFootprint()
        }
        catch {
            case _: InterruptedException => onInterrupt()
            case throwable: Throwable => onFailure(throwable.getMessage)
        }
    }

    private def measureMemoryFootprint(): Unit = {
        logger.log("Waiting for %d solvers to launch".format(workersRemaining.getCount))
        workersRemaining.await()
        if solver == null then {
            throw new IllegalStateException("No solver was set")
        }
        val (memoryFootprintInBytes, overhead) =
            logger.withTimedLogScope("Computing memory footprint using JOL") {
                GraphLayout.parseInstance(solver).totalSize()
            }
        if maybeRuntimeLimitInMillis.isDefined then {
            val newRuntimeLimit = Duration.ofMillis(maybeRuntimeLimitInMillis.get.addAndGet(overhead.toMillis))
            logger.log("Increased runtime limit to %s".format(DurationFormatter.format(newRuntimeLimit)))
        }
        logger.log("Memory footprint: %d bytes".format(memoryFootprintInBytes))
        maybeMemoryFootprintInBytes.complete(Option(memoryFootprintInBytes))
        memoryFootprintMeasured.countDown()
    }

    private def onInterrupt(): Unit = {
        logger.log("Measurement interrupted")
        memoryFootprintMeasured.countDown()
        maybeMemoryFootprintInBytes.complete(None)
    }

    private def onFailure(msg: String): Unit = {
        logger.log("Measurement failed: %s".format(msg))
        memoryFootprintMeasured.countDown()
        maybeMemoryFootprintInBytes.complete(None)
    }

}
