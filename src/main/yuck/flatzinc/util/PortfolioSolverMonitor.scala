package yuck.flatzinc.util

import scala.collection.*

import yuck.annealing.{AnnealingMonitor, AnnealingResult}
import yuck.core.{AnyVariable, IteratedLocalSearchMonitor, LocalSearchMonitor, LocalSearchResult, Result, SolverMonitor, SolverMonitoring}
import yuck.fj.{FeasibilityJumpMonitor, FeasibilityJumpResult}
import yuck.util.arm.ManagedResource

/**
 * Dispatches messages to the given monitors.
 *
 * Identifies recipients by considering the types of messages and monitors.
 *
 * This is not a real monitor, so for use with a solver it needs wrapping.
 *
 * @author Michael Marte
 */
final class PortfolioSolverMonitor(monitors: immutable.Seq[SolverMonitoring[?]]) extends ManagedResource {

    override def open() = {
        monitors.foreach(_.open())
    }

    override def close() = {
        monitors.foreach(_.close())
    }

    def onSolverLaunched(result: Result) = {
        for (monitor <- monitors) {
            (result, monitor) match {
                case (result: AnnealingResult, monitor: AnnealingMonitor) => monitor.onSolverLaunched(result)
                case (result: FeasibilityJumpResult, monitor: FeasibilityJumpMonitor) => monitor.onSolverLaunched(result)
                case (result: LocalSearchResult, monitor: IteratedLocalSearchMonitor) => monitor.onSolverLaunched(result)
                case (result: LocalSearchResult, monitor: LocalSearchMonitor) => monitor.onSolverLaunched(result)
                case (_, monitor: SolverMonitor) => monitor.onSolverLaunched(result)
                case _ =>
            }
        }
    }

    def onSolverSuspended(result: Result) = {
        for (monitor <- monitors) {
            (result, monitor) match {
                case (result: AnnealingResult, monitor: AnnealingMonitor) => monitor.onSolverSuspended(result)
                case (result: FeasibilityJumpResult, monitor: FeasibilityJumpMonitor) => monitor.onSolverSuspended(result)
                case (result: LocalSearchResult, monitor: IteratedLocalSearchMonitor) => monitor.onSolverSuspended(result)
                case (result: LocalSearchResult, monitor: LocalSearchMonitor) => monitor.onSolverSuspended(result)
                case (_, monitor: SolverMonitor) => monitor.onSolverSuspended(result)
                case _ =>
            }
        }
    }

    def onSolverResumed(result: Result) = {
        for (monitor <- monitors) {
            (result, monitor) match {
                case (result: AnnealingResult, monitor: AnnealingMonitor) => monitor.onSolverResumed(result)
                case (result: FeasibilityJumpResult, monitor: FeasibilityJumpMonitor) => monitor.onSolverResumed(result)
                case (result: LocalSearchResult, monitor: IteratedLocalSearchMonitor) => monitor.onSolverResumed(result)
                case (result: LocalSearchResult, monitor: LocalSearchMonitor) => monitor.onSolverResumed(result)
                case (_, monitor: SolverMonitor) => monitor.onSolverResumed(result)
                case _ =>
            }
        }
    }

    def onSolverFinished(result: Result) = {
        for (monitor <- monitors) {
            (result, monitor) match {
                case (result: AnnealingResult, monitor: AnnealingMonitor) => monitor.onSolverFinished(result)
                case (result: FeasibilityJumpResult, monitor: FeasibilityJumpMonitor) => monitor.onSolverFinished(result)
                case (result: LocalSearchResult, monitor: IteratedLocalSearchMonitor) => monitor.onSolverFinished(result)
                case (result: LocalSearchResult, monitor: LocalSearchMonitor) => monitor.onSolverFinished(result)
                case (_, monitor: SolverMonitor) => monitor.onSolverFinished(result)
                case _ =>
            }
        }
    }

    def onBetterProposal(result: Result) = {
        for (monitor <- monitors) {
            (result, monitor) match {
                case (result: AnnealingResult, monitor: AnnealingMonitor) => monitor.onBetterProposal(result)
                case (result: FeasibilityJumpResult, monitor: FeasibilityJumpMonitor) => monitor.onBetterProposal(result)
                case (result: LocalSearchResult, monitor: IteratedLocalSearchMonitor) => monitor.onBetterProposal(result)
                case (result: LocalSearchResult, monitor: LocalSearchMonitor) => monitor.onBetterProposal(result)
                case (_, monitor: SolverMonitor) => monitor.onBetterProposal(result)
                case _ =>
            }
        }
    }

    def onObjectiveTightened(result: Result, x: AnyVariable) = {
        for (monitor <- monitors) {
            (result, monitor) match {
                case (result: AnnealingResult, monitor: AnnealingMonitor) => monitor.onObjectiveTightened(result, x)
                case (result: FeasibilityJumpResult, monitor: FeasibilityJumpMonitor) => monitor.onObjectiveTightened(result, x)
                case (result: LocalSearchResult, monitor: IteratedLocalSearchMonitor) => monitor.onObjectiveTightened(result, x)
                case (result: LocalSearchResult, monitor: LocalSearchMonitor) => monitor.onObjectiveTightened(result, x)
                case _ =>
            }
        }
    }

    def onNextRound(result: Result) = {
        for (monitor <- monitors) {
            (result, monitor) match {
                case (result: AnnealingResult, monitor: AnnealingMonitor) => monitor.onNextRound(result)
                case (result: FeasibilityJumpResult, monitor: FeasibilityJumpMonitor) => monitor.onNextRound(result)
                case (result: LocalSearchResult, monitor: IteratedLocalSearchMonitor) => monitor.onNextRound(result)
                case _ =>
            }
        }
    }

    def onReheatingStarted(result: AnnealingResult) = {
        for (monitor <- monitors) {
            monitor match {
                case annealingMonitor: AnnealingMonitor => annealingMonitor.onReheatingStarted(result)
                case _ =>
            }
        }
    }

    def onReheatingFinished(result: AnnealingResult) = {
        for (monitor <- monitors) {
            monitor match {
                case annealingMonitor: AnnealingMonitor => annealingMonitor.onReheatingFinished(result)
                case _ =>
            }
        }
    }

    def onScheduleRestarted(result: AnnealingResult) = {
        for (monitor <- monitors) {
            monitor match {
                case annealingMonitor: AnnealingMonitor => annealingMonitor.onScheduleRestarted(result)
                case _ =>
            }
        }
    }

    def onPerturbation(result: FeasibilityJumpResult) = {
        for (monitor <- monitors) {
            monitor match {
                case feasibilityJumpMonitor: FeasibilityJumpMonitor => feasibilityJumpMonitor.onPerturbation(result)
                case _ =>
            }
        }
    }

}
