package yuck.flatzinc.test.util

import scala.collection._
import scala.language.implicitConversions
import scala.reflect.ClassTag

import yuck.core._
import yuck.flatzinc.FlatZincSolverConfiguration
import yuck.flatzinc.compiler.FlatZincCompilerResult

/**
 * @author Michael Marte
 *
 */
abstract class FrontEndTest extends MiniZincBasedTest {

    protected val task =
        MiniZincTestTask(
            directoryLayout = MiniZincExamplesLayout,
            suitePath = "resources/mzn/tests/front-end-tests",
            solverConfiguration = FlatZincSolverConfiguration(restartLimit = 0, pruneConstraintNetwork = false),
            maybeRuntimeLimitInSeconds = Some(10),
            assertWhenUnsolved = true,
            reusePreviousTestResult = false,
            createDotFile = true)

    protected def neighbourhood(result: Result): Neighbourhood =
        result.maybeUserData.get.asInstanceOf[FlatZincCompilerResult].maybeNeighbourhood.get

    protected def violation(result: Result): BooleanValue =
        result.costsOfBestProposal match {
            case violation: BooleanValue => violation
            case costs: PolymorphicListValue => costs.value(0).asInstanceOf[BooleanValue]
        }

    protected def quality(result: Result, i: Int): AnyValue =
        result.costsOfBestProposal.asInstanceOf[PolymorphicListValue].value(i)

    protected def quality(result: Result): AnyValue =
        quality(result, 1)

    protected def searchVariables(result: Result): Set[AnyVariable] =
        result.space.searchVariables

    protected def numberOfConstraints[T <: Constraint](result: Result)(implicit classTag: ClassTag[T]): Int =
        result.space.numberOfConstraints(classTag.runtimeClass.isInstance)

    protected implicit def createTask(problemName: String): MiniZincTestTask = task.copy(problemName = problemName)

}
