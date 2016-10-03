package yuck.flatzinc.runner

import yuck.annealing.AnnealingMonitor
import yuck.annealing.AnnealingResult
import yuck.core.Costs

/**
 * @author Michael Marte
 *
 */
final class FlatZincSolverMonitor extends AnnealingMonitor {

    private var costsOfBestSolution: Costs = null

    override def onBetterProposal(result: AnnealingResult) {
        synchronized {
            if (result.isSolution &&
                (costsOfBestSolution == null ||
                 result.objective.isLowerThan(result.costsOfBestProposal, costsOfBestSolution)))
            {
                costsOfBestSolution = result.costsOfBestProposal
                new FlatZincResultFormatter(result).call.foreach(println)
            }
        }
    }

}
