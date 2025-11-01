package yuck.flatzinc.util

import yuck.core.{Costs, Result, SolverMonitor}
import yuck.util.logging.LazyLogger

/**
 * Emits a log message when a better proposal is found.
 *
 * @author Michael Marte
 *
 */
final class BestProposalLogger(logger: LazyLogger) extends SolverMonitor {

    private var costsOfBestProposal: Costs = null

    override def onBetterProposal(result: Result) = {
        synchronized {
            if (costsOfBestProposal.eq(null) ||
                result.objective.isLowerThan(result.costsOfBestProposal, costsOfBestProposal))
            {
                costsOfBestProposal = result.costsOfBestProposal
                logger.log("Improved global proposal quality to %s".format(result.costsOfBestProposal))
                if (result.isGoodEnough) {
                    logger.log("Objective achieved")
                }
            }
        }
    }

}
