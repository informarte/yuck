package yuck.flatzinc.test.util

import yuck.core.{Result, SolverMonitor}

/**
 * Tracks the state of solvers and throws when an unexpected state transition occurs.
 *
 * @author Michael Marte
 */
final class SolverStateTracker extends SolverMonitor {

    private trait ThreadState
    private case object ThreadIsIdle extends ThreadState
    private case object SolverIsRunnable extends ThreadState
    private case object SolverIsRunning extends ThreadState

    private val solverState = new ThreadLocal[ThreadState] {
        override def initialValue = ThreadIsIdle
    }

    override def onSolverLaunched(result: Result) = {
        require(solverState.get == ThreadIsIdle)
        solverState.set(SolverIsRunning)
    }

    override def onSolverSuspended(result: Result) = {
        require(solverState.get == SolverIsRunning)
        solverState.set(SolverIsRunnable)
    }

    override def onSolverResumed(result: Result) = {
        require(solverState.get == SolverIsRunnable)
        solverState.set(SolverIsRunning)
    }

    override def onSolverFinished(result: Result) = {
        require(solverState.get == SolverIsRunning)
        solverState.set(ThreadIsIdle)
    }

}
