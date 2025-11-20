package yuck.core

import yuck.core.Result
import yuck.util.arm.ManagedResource

/**
 * An interface (with empty default implementation) that solvers can use to communicate
 * special events to the outside world.
 */
trait SolverMonitoring[Result <: yuck.core.Result] extends ManagedResource {
    override def open() = {}
    override def close() = {}
    def onSolverLaunched(result: Result): Unit = {}
    def onSolverSuspended(result: Result): Unit = {}
    def onSolverResumed(result: Result): Unit = {}
    def onSolverFinished(result: Result): Unit = {}
    def onBetterProposal(result: Result): Unit = {}
}

class SolverMonitor extends SolverMonitoring[Result]
