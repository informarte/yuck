package yuck.flatzinc.test

import scala.language.implicitConversions

import yuck.core._
import yuck.flatzinc.FlatZincSolverConfiguration
import yuck.flatzinc.compiler.FlatZincCompilerResult
import yuck.flatzinc.test.util._

/**
 * @author Michael Marte
 *
 */
abstract class FrontEndTest extends MiniZincBasedTest {

    protected val task =
        MiniZincTestTask(
            directoryLayout = MiniZincExamplesLayout,
            suitePath = "resources/mzn/tests/front-end-tests",
            solverConfiguration = FlatZincSolverConfiguration(restartLimit = 0),
            maybeRuntimeLimitInSeconds = Some(10),
            assertWhenUnsolved = true,
            reusePreviousTestResult = false,
            exportDot = true)

    protected def neighbourhood(result: Result): Neighbourhood =
        result.maybeUserData.get.asInstanceOf[FlatZincCompilerResult].maybeNeighbourhood.get

    protected def quality(result: Result, i: Int): AnyValue =
        result.costsOfBestProposal.asInstanceOf[PolymorphicListValue].value(i)

    protected def quality(result: Result): AnyValue =
        quality(result, 1)

    protected implicit def createTask(problemName: String): MiniZincTestTask = task.copy(problemName = problemName)

}
