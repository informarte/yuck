package yuck.flatzinc.runner

import yuck.annealing.StandardAnnealingMonitor
import yuck.annealing.AnnealingResult
import yuck.core.Costs
import yuck.util.logging.LazyLogger

/**
 * @author Michael Marte
 *
 */
final class FlatZincSolverMonitor(logger: LazyLogger) extends StandardAnnealingMonitor(logger) {

    private var costsOfBestSolution: Costs = null

    override def onBetterProposal(result: AnnealingResult) = {
        synchronized {
            super.onBetterProposal(result)
            if (result.isSolution &&
                (costsOfBestSolution == null ||
                 result.objective.isLowerThan(result.costsOfBestProposal, costsOfBestSolution)))
            {
                costsOfBestSolution = result.costsOfBestProposal
                new FlatZincResultFormatter(result).call().foreach(println)
            }
        }
    }

}
