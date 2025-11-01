package yuck.flatzinc.test.util

import yuck.core.{Result, SolverMonitor}
import yuck.flatzinc.test.util.SourceFormat.MiniZinc
import yuck.util.logging.LazyLogger
import yuck.util.logging.LogLevel.FineLogLevel

/**
 * Verifies every solution.
 *
 * @author Michael Marte
 */
final class CorrectnessSentinel
    (task: ZincTestTask, spoilResult: Result => Result, logger: LazyLogger)
    extends SolverMonitor
{

    require(task.sourceFormat == MiniZinc)

    override def onBetterProposal(result: Result) = {
        if (result.isSolution) {
            verifySolution(task, result)
        }
    }

    private def verifySolution(task: ZincTestTask, result: Result): Unit = {
        logger.criticalSection {
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

}
