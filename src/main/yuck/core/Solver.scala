package yuck.core

import java.util.concurrent._

import scala.collection._

import yuck.util.arm._
import yuck.util.logging._

/**
 * @author Michael Marte
 *
 */
trait SolverLifecycle {

    /**
     * Asks the solver to suspend itself and return the best result found so far.
     *
     * Interrupting interrupted or finished solvers is allowed.
     *
     * (The Java interrupt mechanism (Thread.interrupt, Future.cancel) is not suitable
     * for anytime algorithms: When a Future gets cancelled in one way or another, it
     * will yield no result, even when the interrupted computation has provided one.
     * Therefore we have to provide and use our own interruption mechanism.)
     */
    def interrupt: Unit

    /**
     * To be called before resuming the solver (by means of call).
     *
     * The solver must have been interrupted and must not have finished.
     */
    def resume: Unit

    /** Returns true when the solver was interrupted and has not yet been resumed. */
    def wasInterrupted: Boolean

    /** Do not call on a running solver! */
    def hasFinished: Boolean

}

/**
 * @author Michael Marte
 *
 */
abstract class Solver extends Callable[Result] with SolverLifecycle {

    /** Return the solver's name. */
    def name: String = this.getClass.getName

    /**
     * Runs the solver and returns the best result it came across.
     *
     * When the solver gets interrupted, call shall terminate asap with or without result.
     *
     * When the solver finds a solution that is good enough (according to its objective),
     * call shall immediately return it.
     *
     * call shall not be used on finished solvers.
     */
    override def call: Result

}

/**
 * Implements basic support for dealing with interrupts.
 *
 * @author Michael Marte
 */
trait StandardSolverInterruptionSupport extends SolverLifecycle {

    private var interrupted = false

    override def interrupt {
        interrupted = true
    }

    override def resume {
        require(! hasFinished)
        require(wasInterrupted)
        interrupted = false
    }

    override def wasInterrupted = interrupted

}

/**
 * @author Michael Marte
 *
 */
abstract class SolverGenerator extends Callable[Solver] {
    /** Should return the name of the unborn solver. */
    def solverName: String
}

/**
 * An interface (with empty default implementation) that solvers can use to communicate
 * special events to the outside world.
 *
 * @author Michael Marte
 */
class SolverMonitor[ResultImpl <: Result] {
    def onSolverLaunched(result: ResultImpl) {}
    def onSolverSuspended(result: ResultImpl) {}
    def onSolverResumed(result: ResultImpl) {}
    def onSolverFinished(result: ResultImpl) {}
    def onBetterProposal(result: ResultImpl) {}
}

/**
 * A solver that is always finished and hence cannot be run.
 *
 * @author Michael Marte
 */
final object FinishedSolver extends Solver with StandardSolverInterruptionSupport {
    override def hasFinished = true
    override def call = {
        require(! hasFinished, "Use a new solver")
        null
    }
}

/**
 * Interrupts the given solver after the given runtime.
 *
 * Stops the watch on interruption and resumes it on resumption.
 *
 * @author Michael Marte
 */
final class TimeboxedSolver(
    solver: Solver,
    runtimeInSeconds: Int,
    logger: LazyLogger)
    extends Solver
{

    private var remainingRuntimeInMillis: Long = runtimeInSeconds * 1000

    override def name = solver.name

    override def hasFinished = solver.hasFinished && remainingRuntimeInMillis > 0

    override def call = {
        require(! hasFinished)
        val watchdogThread = new Thread {
            override def run {
                val t0 = System.currentTimeMillis
                try {
                    Thread.sleep(remainingRuntimeInMillis)
                }
                catch {
                    case error: InterruptedException =>
                        logger.logg("Interrupted")
                }
                finally {
                    val t1 = System.currentTimeMillis
                    remainingRuntimeInMillis -= (t1 - t0)
                }
                if (remainingRuntimeInMillis <= 0) {
                    logger.log("Out of time, asking %s to stop".format(solver.name))
                    solver.interrupt
                }
            }
        }
        val result =
            scoped(new TransientThreadRenaming(watchdogThread, "watchdog for %s".format(solver.name))) {
                scoped(new ManagedThread(watchdogThread, logger)) {
                    solver.call
                }
            }
        result
    }

    override def interrupt {
        solver.interrupt
    }

    override def resume {
        solver.resume
    }

    override def wasInterrupted = solver.wasInterrupted

}

/**
 * When called the first time, the given solver generator is used to obtain the solver to run.
 *
 * The solver is cached for the purpose of resumption.
 *
 * When the solver has finished, it gets replaced by a mock to free memory.
 *
 * Notice that the generation part cannot be interrupted.
 *
 * @author Michael Marte
 */
final class OnDemandGeneratedSolver(
    solverGenerator: SolverGenerator,
    logger: LazyLogger)
    extends Solver
    with StandardSolverInterruptionSupport
{

    private var solver: Solver = null

    override def name = solverGenerator.solverName

    override def hasFinished = solver != null && solver.hasFinished

    override def call = {
        require(! hasFinished)
        var result: Result = null
        if (solver == null) {
            if (wasInterrupted) {
                logger.loggg("Interrupted, not generating solver")
            } else {
                logger.withTimedLogScope("Generating solver") {
                    solver = solverGenerator.call
                }
            }
        }
        if (! wasInterrupted) {
            result =
                logger.withTimedLogScope("Running solver") {
                    solver.call
                }
            if (solver.hasFinished) {
                // replace solver by mock to free memory
                solver = FinishedSolver
            }
        }
        result
    }

    override def interrupt {
        super.interrupt
        if (solver != null) solver.interrupt
    }

    override def resume {
        super.resume
        if (solver != null) solver.resume
    }

}

/**
 * Creates a thread pool of the given size and uses it to run the given solvers.
 *
 * Keeps track of the best proposal and provides it as the overall result of optimization.
 *
 * Interrupts the given solvers when a solver has achieved the optimization goal
 * or has thrown an exception.
 *
 * The given solvers may use different models, search methods, and strategies but they
 * must pursue the same optimization goal.
 *
 * @author Michael Marte
 */
final class ParallelSolver(
    solvers: Seq[Solver],
    threadPoolSize: Int,
    override val name: String,
    logger: LazyLogger)
    extends Solver
    with StandardSolverInterruptionSupport
{

    require(! solvers.isEmpty)
    assert(threadPoolSize > 0)

    private var bestResult: Result = null
    private val lock = new locks.ReentrantLock
    private val indentation = logger.currentIndentation

    override def hasFinished =
        (bestResult != null && bestResult.isGoodEnough) || solvers.forall(_.hasFinished)

    private class SolverRunner(child: Solver) extends Runnable {
        override def run {
            if (! child.wasInterrupted) {
                scoped(new TransientThreadRenaming(Thread.currentThread, child.name)) {
                    scoped(new LogScope(logger, indentation)) {
                        logger.withTimedLogScope("Running child") {
                            val result = child.call
                            if (result != null) {
                                criticalSection(lock) {
                                    if (bestResult == null || result.isBetterThan(bestResult)) {
                                        bestResult = result
                                        if (bestResult.isGoodEnough) {
                                            logger.log("Objective achieved")
                                            interruptSolvers
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    override def call = {
        require(! hasFinished)
        if (! wasInterrupted) {
            val threadPool = Executors.newFixedThreadPool(threadPoolSize)
            scoped(new ManagedExecutorService(threadPool, logger)) {
                val futureResults =
                    solvers.filter(! _.hasFinished).map(new SolverRunner(_)).map(threadPool.submit(_))
                try {
                    // throw up exceptions
                    val results = futureResults.map(_.get)
                }
                finally {
                    interrupt
                }
            }
        }
        bestResult
    }

    override def interrupt {
        super.interrupt
        interruptSolvers
    }

    private def interruptSolvers {
        logger.log("Interrupting solvers")
        solvers.foreach(_.interrupt)
    }

    override def resume {
        super.resume
        resumePendingSolvers
    }

    private def resumePendingSolvers {
        logger.log("Resuming pending solvers")
        for (solver <- solvers if ! solver.hasFinished) solver.resume
    }

}
