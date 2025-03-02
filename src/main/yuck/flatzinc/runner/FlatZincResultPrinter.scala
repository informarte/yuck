package yuck.flatzinc.runner

import java.util.concurrent.atomic.AtomicReference

import yuck.annealing.{AnnealingMonitor, AnnealingResult}
import yuck.core.Costs
import yuck.flatzinc.ast.FlatZincAst

/**
 * A monitor for printing solutions.
 *
 * Designed to work in its own thread.
 *
 * @author Michael Marte
 */
final class FlatZincResultPrinter
    (ast: FlatZincAst, throttlingIntervalInMillis: Int)
    extends AnnealingMonitor
    with Runnable
{

    private val solutionFormatter = new FlatZincResultFormatter(ast)

    private var costsOfBestSolution: Costs = null

    private val nextResult = new AtomicReference[FlatZincResult]()

    override def run() = {
        if (throttlingIntervalInMillis > 0) {
            var interrupted = false
            while (! interrupted) {
                try {
                    Thread.sleep(throttlingIntervalInMillis)
                }
                catch {
                    case _: InterruptedException => interrupted = true
                }
                flush()
            }
        }
    }

    def flush(): Unit = {
        val result = nextResult.getAndSet(null)
        if (result != null) {
            solutionFormatter(result).foreach(println)
        }
    }

    override def onBetterProposal(result: AnnealingResult) = {
        if (result.isSolution) {
            synchronized {
                if (costsOfBestSolution.eq(null) ||
                    result.objective.isLowerThan(result.costsOfBestProposal, costsOfBestSolution))
                {
                    costsOfBestSolution = result.costsOfBestProposal
                    val fznResult = new FlatZincResult(result)
                    if (throttlingIntervalInMillis > 0) {
                        nextResult.set(fznResult)
                    } else {
                        solutionFormatter(fznResult).foreach(println)
                    }
                }
            }
        }
    }

}
