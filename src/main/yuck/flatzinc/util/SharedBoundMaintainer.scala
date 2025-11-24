package yuck.flatzinc.util

import java.util.concurrent.atomic.AtomicReference

import yuck.core.{Costs, Result, SolverMonitor}

/**
 * Tracks solvers in order to update the cost vector of the best solution seen so far.
 */
final class SharedBoundMaintainer(holder: AtomicReference[Costs]) extends SolverMonitor {

    override def onBetterProposal(result: Result) = {
        if result.isSolution then {
            holder.accumulateAndGet(
                result.costsOfBestProposal,
                (currentBound, newBound) =>
                    if currentBound == null || result.objective.isLowerThan(newBound, currentBound)
                    then newBound
                    else currentBound)
        }
    }

}
