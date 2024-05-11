package yuck.flatzinc.test.util

import scala.collection.*
import scala.language.implicitConversions
import scala.reflect.ClassTag

import yuck.annealing.AnnealingResult
import yuck.core.{given, *}
import yuck.flatzinc.FlatZincSolverConfiguration
import yuck.flatzinc.compiler.FlatZincCompilerResult
import yuck.flatzinc.test.util.TestDataDirectoryLayout.*

/**
 * @author Michael Marte
 *
 */
abstract class FrontEndTest extends ZincBasedTest {

    protected val task =
        ZincTestTask(
            directoryLayout = MiniZincExamplesLayout,
            suitePath = "resources/mzn/tests/front-end-tests",
            solverConfiguration = FlatZincSolverConfiguration(attachGoals = true, restartLimit = 0),
            maybeRuntimeLimitInSeconds = Some(10),
            throwWhenUnsolved = true,
            reusePreviousTestResult = false,
            miniZincCompilerRenamesVariables = false,
            createDotFile = true)

    extension (result: Result) {

        protected def neighbourhood: Neighbourhood =
            compilerResult.maybeNeighbourhood.get

        protected def violation: BooleanValue =
            result.costsOfBestProposal match {
                case violation: BooleanValue => violation
                case costs: PolymorphicListValue => costs.value(0).asInstanceOf[BooleanValue]
            }

        protected def quality(i: Int): AnyValue =
            result.costsOfBestProposal.asInstanceOf[PolymorphicListValue].value(i)

        protected def quality: AnyValue =
            result.quality(1)

        protected def assignment: SearchState =
            result.space.searchState

        protected def warmStartWasPerformed: Boolean =
            compilerResult.performWarmStart

        protected def searchWasPerformed: Boolean =
            ! result.asInstanceOf[AnnealingResult].roundLogs.isEmpty

        protected def compilerResult: FlatZincCompilerResult =
            result.maybeUserData.get.asInstanceOf[FlatZincCompilerResult]

    }

    protected implicit def createTask(problemName: String): ZincTestTask =
        task.copy(problemName = problemName)

    protected final def wasIntroducedByMiniZincCompiler(x: AnyVariable): Boolean =
        x.name.startsWith("X_INTRODUCED")

    protected final def wasIntroducedByYuck(x: AnyVariable): Boolean =
        x.name.isEmpty || x.name == "_YUCK_LB" || x.name == "_YUCK_UB"

    protected final def isUserDefined(x: AnyVariable): Boolean =
        ! wasIntroducedByMiniZincCompiler(x) && ! wasIntroducedByYuck(x)

}
