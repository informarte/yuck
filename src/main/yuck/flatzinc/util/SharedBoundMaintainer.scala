package yuck.flatzinc.util

import java.util.concurrent.atomic.AtomicReference

import yuck.annealing.{AnnealingMonitor, AnnealingResult}
import yuck.core.Costs

/**
 * Tracks solvers in order to update the cost vector of the best solution seen so far.
 *
 * @author Michael Marte
 *
 */
final class SharedBoundMaintainer(holder: AtomicReference[Costs]) extends AnnealingMonitor {

    override def onBetterProposal(result: AnnealingResult) = {
        if (result.isSolution) {
            holder.accumulateAndGet(
                result.costsOfBestProposal,
                (currentBound, newBound) =>
                    if currentBound == null || result.objective.isLowerThan(newBound, currentBound)
                    then newBound
                    else currentBound)
        }
    }

}
