package yuck.flatzinc.test

import org.junit.jupiter.api.Test
import org.junit.platform.suite.api.IncludeTags

import yuck.core.{AnyVariable, LocalSearchMonitor, LocalSearchResult}
import yuck.flatzinc.test.util.*
import yuck.flatzinc.test.util.ProblemType.*
import yuck.util.logging.LogLevel.FineLogLevel

final class ProgressiveTighteningTest extends FrontEndTest {

    private final class TighteningCounter extends LocalSearchMonitor {
        var n = 0
        override def onObjectiveTightened(result: LocalSearchResult, x: AnyVariable): Unit = {
            n += 1
        }
    }

    private val tighteningCounter = new TighteningCounter

    private val tighteningTask =
        task.copy(
            solverConfiguration = task.solverConfiguration.copy(useProgressiveTightening = true),
            additionalMonitors = List(tighteningCounter),
            logLevel = FineLogLevel,
            miniZincCompilerRenamesVariables = true)

    @Test
    @IncludeTags(Array(MinimizationProblem))
    def testProgressiveTighteningWhenMinimizing(): Unit = {
        solve(tighteningTask.copy(problemName = "progressive_tightening_when_minimizing_test"))
        assertGt(tighteningCounter.n, 1)
    }

    @Test
    @IncludeTags(Array(MaximizationProblem))
    def testProgressiveTighteningWhenMaximizing(): Unit = {
        solve(tighteningTask.copy(problemName = "progressive_tightening_when_maximizing_test"))
        assertGt(tighteningCounter.n, 1)
    }

}
