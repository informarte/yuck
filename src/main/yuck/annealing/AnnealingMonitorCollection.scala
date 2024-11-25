package yuck.annealing

import scala.collection.*

import yuck.core.AnyVariable

/**
 * @author Michael Marte
 *
 */
final class AnnealingMonitorCollection(monitors: immutable.Seq[AnnealingMonitor]) extends AnnealingMonitor {

    override def open() = {
        monitors.foreach(_.open())
    }

    override def close() = {
        monitors.foreach(_.close())
    }

    override def onSolverLaunched(result: AnnealingResult) = {
        monitors.foreach(_.onSolverLaunched(result))
    }

    override def onSolverSuspended(result: AnnealingResult) = {
        monitors.foreach(_.onSolverSuspended(result))
    }

    override def onSolverResumed(result: AnnealingResult) = {
        monitors.foreach(_.onSolverResumed(result))
    }

    override def onSolverFinished(result: AnnealingResult) = {
        monitors.foreach(_.onSolverFinished(result))
    }

    override def onBetterProposal(result: AnnealingResult) = {
        monitors.foreach(_.onBetterProposal(result))
    }

    override def onObjectiveTightened(x: AnyVariable) = {
        monitors.foreach(_.onObjectiveTightened(x))
    }


    override def onNextRound(result: AnnealingResult) = {
        monitors.foreach(_.onNextRound(result))
    }

    override def onReheatingStarted(result: AnnealingResult) = {
        monitors.foreach(_.onReheatingStarted(result))
    }

    override def onReheatingFinished(result: AnnealingResult) = {
        monitors.foreach(_.onReheatingFinished(result))
    }

}
