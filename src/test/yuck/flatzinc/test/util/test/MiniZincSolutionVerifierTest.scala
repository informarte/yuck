package yuck.flatzinc.test.util.test

import org.junit.jupiter.api.Test
import org.junit.jupiter.params.ParameterizedClass
import org.junit.jupiter.params.provider.MethodSource

import yuck.SolvingMethod
import yuck.core.*
import yuck.flatzinc.test.util.*
import yuck.flatzinc.test.util.TestDataDirectoryLayout.*
import yuck.flatzinc.test.util.VerificationFrequency.*
import yuck.test.*

@ParameterizedClass
@MethodSource(Array("parameters"))
class MiniZincSolutionVerifierTest(simulateBadSolver: Boolean, verificationFrequency: VerificationFrequency) extends ZincBasedTest {

    final class SpoiledResult(result: Result) extends Result {
        override val maybeUserData = result.maybeUserData
        override val solverName = result.solverName
        override val objective = result.objective
        override val bestProposal = {
            val varDir = result.bestProposal.mappedVariables.map(x => (x.name, x)).toMap
            val modifiedSolution = new HashMapBackedAssignment(result.bestProposal)
            modifiedSolution.setValue(varDir("x").asInstanceOf[IntegerVariable], Ten)
            modifiedSolution.setValue(varDir("y").asInstanceOf[IntegerVariable], Ten)
            modifiedSolution
        }
        override val searchWasPerformed = result.searchWasPerformed
        override val runtimeInMillis = result.runtimeInMillis
    }

    override protected def spoilResult(result: Result) =
        if simulateBadSolver then new SpoiledResult(result) else result

    @Test
    def testVerification(): Unit = {
        val task =
            ZincTestTask(
                directoryLayout = MiniZincExamplesLayout,
                suitePath = "resources/mzn/tests/test-util-tests",
                problemName = "verification-test",
                solverConfiguration =
                    ZincTestTask().solverConfiguration.copy(
                        numberOfSolvers = 1,
                        pruneConstraintNetwork = false,
                        maybePreferredSolvingMethod = Some(SolvingMethod.SimulatedAnnealing),
                        maybeRuntimeLimitInSeconds = Some(10)),
                throwWhenUnsolved = true,
                verificationFrequency = verificationFrequency)
        if simulateBadSolver && verificationFrequency != NoVerification then {
            assertThrows(solve(task), classOf[SolutionNotVerifiedException])
        } else {
            solve(task)
        }
    }

}

object MiniZincSolutionVerifierTest {

    private def configurations =
        for simulateBadSolver <- List(false, true)
            verificationFrequency <- List(NoVerification, VerifyOnlyLastSolution, VerifyEverySolution)
        yield
            Array(simulateBadSolver, verificationFrequency)

    def parameters = configurations.toArray

}
