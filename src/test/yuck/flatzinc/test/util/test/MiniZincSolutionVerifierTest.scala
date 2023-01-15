package yuck.flatzinc.test.util.test

import org.junit.*

import scala.jdk.CollectionConverters.*

import yuck.core.{given, *}
import yuck.flatzinc.FlatZincSolverConfiguration
import yuck.flatzinc.test.util.*

/**
 * @author Michael Marte
 *
 */
@runner.RunWith(classOf[runners.Parameterized])
class MiniZincSolutionVerifierTest(simulateBadSolver: Boolean) extends ZincBasedTest {

    protected override val logToConsole = false

    protected override def onSolved(result: Result) = {
        if (simulateBadSolver) {
            val varDir = result.bestProposal.mappedVariables.map(x => (x.name, x)).toMap
            val modifiedSolution = new HashMapBackedAssignment(result.bestProposal)
            modifiedSolution.setValue(varDir("x").asInstanceOf[IntegerVariable], Ten)
            modifiedSolution.setValue(varDir("y").asInstanceOf[IntegerVariable], Ten)
            result.bestProposal = modifiedSolution
        }
    }

    @Test
    def testVerification(): Unit = {
        val task =
            ZincTestTask(
                directoryLayout = MiniZincExamplesLayout,
                suitePath = "resources/mzn/tests/test-util-tests",
                problemName = "verification-test",
                solverConfiguration = FlatZincSolverConfiguration(restartLimit = 0, pruneConstraintNetwork = false),
                maybeRuntimeLimitInSeconds = Some(10),
                throwWhenUnsolved = true,
                reusePreviousTestResult = false)
        if (simulateBadSolver) {
            assertEx(solve(task), classOf[SolutionNotVerifiedException])
        } else {
            solve(task)
        }
    }

}

/**
 * @author Michael Marte
 *
 */
object MiniZincSolutionVerifierTest {

    @runners.Parameterized.Parameters(name = "{index}: {0}")
    def parameters = List(false, true).asJava

}
