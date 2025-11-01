package yuck.flatzinc.runner

import yuck.core.{Result, SearchState}
import yuck.flatzinc.compiler.FlatZincCompilerResult

/**
 * @author Michael Marte
 *
 */
final class FlatZincResult(val compilerResult: FlatZincCompilerResult, val searchState: SearchState) {
    def this(result: Result) =
        this(result.maybeUserData.get.asInstanceOf[FlatZincCompilerResult], result.bestProposal)
}
