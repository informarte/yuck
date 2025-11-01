package yuck.flatzinc.util

import yuck.fj.{FeasibilityJumpMonitor, FeasibilityJumpResult}
import yuck.core.AnyVariable

/**
 * Wraps a PortfolioSolverMonitor instance for use with the feasibility-jump method.
 *
 * @author Michael Marte
 */
final class FeasibilityJumpMonitorFromPortfolioSolverMonitor(monitor: PortfolioSolverMonitor) extends FeasibilityJumpMonitor {

    override def open() = {
        monitor.open()
    }

    override def close() = {
        monitor.close()
    }

    override def onSolverLaunched(result: FeasibilityJumpResult) = {
        monitor.onSolverLaunched(result)
    }

    override def onSolverSuspended(result: FeasibilityJumpResult) = {
        monitor.onSolverSuspended(result)
    }

    override def onSolverResumed(result: FeasibilityJumpResult) = {
        monitor.onSolverResumed(result)
    }

    override def onSolverFinished(result: FeasibilityJumpResult) = {
        monitor.onSolverFinished(result)
    }

    override def onBetterProposal(result: FeasibilityJumpResult) = {
        monitor.onBetterProposal(result)
    }

    override def onObjectiveTightened(result: FeasibilityJumpResult, x: AnyVariable) = {
        monitor.onObjectiveTightened(result, x)
    }

    override def onNextRound(result: FeasibilityJumpResult) = {
        monitor.onNextRound(result)
    }

    override def onPerturbation(result: FeasibilityJumpResult) = {
        monitor.onPerturbation(result)
    }

}
