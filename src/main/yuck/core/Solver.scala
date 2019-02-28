package yuck.core

import java.util.concurrent._

import scala.collection._

import yuck.util.arm._
import yuck.util.logging._

/**
 * @author Michael Marte
 *
 */
final class SolverInterruptedException
extends InterruptedException("Solver was interrupted")

/**
 * @author Michael Marte
 *
 */
abstract class Solver extends Callable[Result] {

    /** Returns the solver's name. */
    def name: String = this.getClass.getName

    /**
     * Runs the solver and returns the best result it came across.
     *
     * When the solver finds a solution that is good enough (according to its objective),
     * call immediately returns it.
     *
     * When the solver gets interrupted in some way, call terminates asap.
     * In case a result is not yet available, call throws a SolverInterruptedException.
     *
     * call is able to resume an unfinished solver which previously got interrupted.
     *
     * call might not work on a finished solver.
     */
    override def call: Result

    /** Do not call on a running solver! */
    def hasFinished: Boolean

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
    def onObjectiveTightened(x: AnyVariable) {}
}

/**
 * A solver that is always finished and hence cannot be run.
 *
 * @author Michael Marte
 */
final object FinishedSolver extends Solver {
    override def hasFinished = true
    override def call = {
        require(! hasFinished, "Use a new solver")
        null
    }
}

/**
 * Interrupts the given solver after reaching the given runtime limit.
 *
 * Stops the watch on interruption and resumes it on resumption.
 *
 * @author Michael Marte
 */
final class TimeboxedSolver(
    solver: Solver,
    runtimeLimitInSeconds: Int,
    logger: LazyLogger,
    sigint: SettableSigint)
    extends Solver
{
    private def solve = solver.call
    private val timebox = new TimeboxedOperation(solve, runtimeLimitInSeconds, sigint, solver.name, logger)
    override def name = solver.name
    override def hasFinished = solver.hasFinished || timebox.isOutOfTime
    override def call = timebox.call
}

/**
 * When called the first time, the given solver generator is used to obtain the solver to run.
 *
 * The solver is cached for the purpose of resumption.
 *
 * When the solver has finished, it gets replaced by a mock to free memory.
 *
 * @author Michael Marte
 */
final class OnDemandGeneratedSolver(
    solverGenerator: SolverGenerator,
    logger: LazyLogger,
    sigint: Sigint)
    extends Solver
{

    private var solver: Solver = null

    override def name = solverGenerator.solverName

    override def hasFinished = solver != null && solver.hasFinished

    override def call = {
        require(! hasFinished)
        if (solver == null) {
            if (sigint.isSet) {
                logger.loggg("Interrupted, not generating solver")
            } else {
                logger.withTimedLogScope("Generating solver") {
                    try {
                        solver = solverGenerator.call
                    }
                    catch {
                        case error: InterruptedException =>
                            logger.log(error.getMessage)
                    }
                }
            }
        }
        if (sigint.isSet) {
            throw new SolverInterruptedException
        } else {
            val result =
                logger.withTimedLogScope("Running solver") {
                    solver.call
                }
            if (solver.hasFinished) {
                // replace solver by mock to free memory
                solver = FinishedSolver
            }
            result
        }
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
    logger: LazyLogger,
    sigint: SettableSigint)
    extends Solver
{

    require(! solvers.isEmpty)
    assert(threadPoolSize > 0)

    private var maybeBestResult: Option[Result] = None
    private val lock = new locks.ReentrantLock
    private val indentation = logger.currentIndentation

    override def hasFinished =
        (maybeBestResult.isDefined && maybeBestResult.get.isGoodEnough) || solvers.forall(_.hasFinished)

    private class SolverRunner(child: Solver) extends Runnable {
        override def run {
            if (! sigint.isSet) {
                scoped(new TransientThreadRenaming(Thread.currentThread, child.name)) {
                    scoped(new LogScope(logger, indentation)) {
                        logger.withTimedLogScope("Running child") {
                            try {
                                val result = child.call
                                criticalSection(lock) {
                                    if (maybeBestResult.isEmpty || result.isBetterThan(maybeBestResult.get)) {
                                        maybeBestResult = Some(result)
                                        if (maybeBestResult.get.isGoodEnough) {
                                            sigint.set
                                        }
                                    }
                                }
                            }
                            catch {
                                case error: InterruptedException =>
                                    logger.log(error.getMessage)
                                case error: Throwable =>
                                    logger.criticalSection {
                                        logger.withLogScope(error.toString) {
                                            error.getStackTrace.foreach(frame => logger.log(frame.toString))
                                        }
                                    }
                                    sigint.set
                                    throw error
                            }
                        }
                    }
                }
            }
        }
    }

    override def call = {
        require(! hasFinished)
        if (! sigint.isSet) {
            val threadPool = Executors.newFixedThreadPool(threadPoolSize)
            scoped(new ManagedExecutorService(threadPool, logger)) {
                val futureResults =
                    solvers.filter(! _.hasFinished).map(new SolverRunner(_)).map(threadPool.submit(_))
                try {
                    // throw up exceptions
                    val results = futureResults.map(_.get)
                }
                finally {
                    sigint.set
                }
            }
        }
        if (maybeBestResult.isEmpty) {
            throw new SolverInterruptedException
        }
        maybeBestResult.get
    }

}
