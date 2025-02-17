package yuck.flatzinc.test.util.test

import scala.collection.*
import scala.jdk.CollectionConverters.*

import org.junit.*

import yuck.annealing.{AnnealingMonitor, AnnealingMonitorCollection, AnnealingResult}
import yuck.core.*
import yuck.flatzinc.FlatZincSolverConfiguration
import yuck.flatzinc.test.util.*
import yuck.flatzinc.test.util.TestDataDirectoryLayout.*
import yuck.flatzinc.test.util.VerificationFrequency.*
import yuck.test.*

/**
 * @author Michael Marte
 *
 */
@runner.RunWith(classOf[runners.Parameterized])
class MiniZincSolutionVerifierTest(simulateBadSolver: Boolean, verificationFrequency: VerificationFrequency) extends ZincBasedTest {

    override protected val logToConsole = false

    class SolutionSpoiler extends AnnealingMonitor {

        override def onBetterProposal(result: AnnealingResult) = {
            val varDir = result.bestProposal.mappedVariables.map(x => (x.name, x)).toMap
            val modifiedSolution = new HashMapBackedAssignment(result.bestProposal)
            modifiedSolution.setValue(varDir("x").asInstanceOf[IntegerVariable], Ten)
            modifiedSolution.setValue(varDir("y").asInstanceOf[IntegerVariable], Ten)
            result.bestProposal = modifiedSolution
        }

    }

    override protected def customizeMonitoring(monitors: Seq[AnnealingMonitor]) =
        if simulateBadSolver then monitors.prepended(new SolutionSpoiler) else monitors

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
                reusePreviousTestResult = false,
                verificationFrequency = verificationFrequency)
        if (simulateBadSolver && verificationFrequency != NoVerification) {
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

    private def configurations =
        for (simulateBadSolver <- List(false, true);
             verificationFrequency <- List(NoVerification, VerifyOnlyLastSolution, VerifyEverySolution))
        yield Vector(simulateBadSolver, verificationFrequency)

    @runners.Parameterized.Parameters(name = "{index}: {0}, {1}")
    def parameters = configurations.map(_.toArray).asJava

}
