package yuck.flatzinc.test.util

import yuck.annealing.{AnnealingMonitor, AnnealingResult}
import yuck.core.*
import yuck.util.logging.LazyLogger
import yuck.util.logging.LogLevel.FineLogLevel

/**
 * A monitor which verifies every solution.
 *
 * @author Michael Marte
 */
final class MiniZincTestMonitor
    (task: ZincTestTask, spoilResult: Result => Result, logger: LazyLogger)
    extends AnnealingMonitor
{

    override def onBetterProposal(result: AnnealingResult) = {
        if (result.isSolution) {
            logger.criticalSection {
                verifySolution(task, result)
            }
        }
    }

    private def verifySolution(task: ZincTestTask, result: Result): Unit = {
        logger.withTimedLogScope("Verifying solution") {
            logger.withRootLogLevel(FineLogLevel) {
                val verifier = new MiniZincSolutionVerifier(task, spoilResult(result), logger)
                if (! verifier.call()) {
                    throw new SolutionNotVerifiedException
                }
            }
        }
    }

}
