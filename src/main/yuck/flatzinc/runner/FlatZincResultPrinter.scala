package yuck.flatzinc.runner

import yuck.annealing.{AnnealingMonitor, AnnealingResult}
import yuck.core.Costs
import yuck.flatzinc.ast.FlatZincAst
import yuck.util.logging.LazyLogger

/**
 * A monitor which prints every solution.
 *
 * @author Michael Marte
 */
final class FlatZincResultPrinter(ast: FlatZincAst, logger: LazyLogger) extends AnnealingMonitor {

    private val solutionFormatter = new FlatZincResultFormatter(ast)

    private var costsOfBestSolution: Costs = null

    override def onBetterProposal(result: AnnealingResult) = {
        synchronized {
            if (result.isSolution &&
                (costsOfBestSolution.eq(null) ||
                 result.objective.isLowerThan(result.costsOfBestProposal, costsOfBestSolution)))
            {
                costsOfBestSolution = result.costsOfBestProposal
                solutionFormatter(result).foreach(println)
            }
        }
    }

}
