package yuck.flatzinc.test

import org.junit.Test
import org.junit.experimental.categories.Category

import yuck.annealing.AnnealingMonitor
import yuck.core.AnyVariable
import yuck.flatzinc.test.util.*
import yuck.util.logging.LogLevel.FineLogLevel

/**
 * @author Michael Marte
 *
 */
final class ProgressiveTighteningTest extends FrontEndTest {

    override protected val logToConsole = false

    private final class TighteningCounter extends AnnealingMonitor {
        var n = 0
        override def onObjectiveTightened(x: AnyVariable) = {
            n += 1
        }
    }

    private val tighteningCounter = new TighteningCounter

    private val tighteningTask =
        task.copy(
            solverConfiguration = task.solverConfiguration.copy(useProgressiveTightening = true),
            additionalMonitors = List(tighteningCounter),
            logLevel = FineLogLevel)

    @Test
    @Category(Array(classOf[MinimizationProblem]))
    def testProgressiveTighteningWhenMinimizing(): Unit = {
        solve(tighteningTask.copy(problemName = "progressive_tightening_when_minimizing_test"))
        assertGt(tighteningCounter.n, 1)
    }

    @Test
    @Category(Array(classOf[MaximizationProblem]))
    def testProgressiveTighteningWhenMaximizing(): Unit = {
        solve(tighteningTask.copy(problemName = "progressive_tightening_when_maximizing_test"))
        assertGt(tighteningCounter.n, 1)
    }

}
