package yuck.flatzinc.runner

import java.util.concurrent.atomic.AtomicReference

import yuck.core.{Costs, Result, SolverMonitor}
import yuck.flatzinc.ast.FlatZincAst

/**
 * Prints solutions in FlatZinc format.
 *
 * Designed to work in its own thread.
 *
 * @author Michael Marte
 */
final class FlatZincResultPrinter
    (ast: FlatZincAst, throttlingIntervalInMillis: Int)
    extends SolverMonitor
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

    override def onBetterProposal(result: Result) = {
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
