package yuck.flatzinc.util

import yuck.annealing.{AnnealingMonitor, AnnealingResult}
import yuck.core.AnyVariable

/**
 * Wraps a PortfolioSolverMonitor instance for use with simulated annealing.
 *
 * @author Michael Marte
 */
final class AnnealingMonitorFromPortfolioSolverMonitor(monitor: PortfolioSolverMonitor) extends AnnealingMonitor {

    override def open() = {
        monitor.open()
    }

    override def close() = {
        monitor.close()
    }

    override def onSolverLaunched(result: AnnealingResult) = {
        monitor.onSolverLaunched(result)
    }

    override def onSolverSuspended(result: AnnealingResult) = {
        monitor.onSolverSuspended(result)
    }

    override def onSolverResumed(result: AnnealingResult) = {
        monitor.onSolverResumed(result)
    }

    override def onSolverFinished(result: AnnealingResult) = {
        monitor.onSolverFinished(result)
    }

    override def onBetterProposal(result: AnnealingResult) = {
        monitor.onBetterProposal(result)
    }

    override def onObjectiveTightened(result: AnnealingResult, x: AnyVariable) = {
        monitor.onObjectiveTightened(result, x)
    }

    override def onNextRound(result: AnnealingResult) = {
        monitor.onNextRound(result)
    }

    override def onReheatingStarted(result: AnnealingResult) = {
        monitor.onReheatingStarted(result)
    }

    override def onReheatingFinished(result: AnnealingResult) = {
        monitor.onReheatingFinished(result)
    }

    override def onScheduleRestarted(result: AnnealingResult) = {
        monitor.onScheduleRestarted(result)
    }

}
