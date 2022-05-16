package yuck.flatzinc.test.util

import scala.collection.*
import scala.language.implicitConversions
import scala.reflect.ClassTag

import yuck.annealing.AnnealingResult
import yuck.core.*
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

    extension (result: Result) {

        protected def neighbourhood: Neighbourhood =
            result.maybeUserData.get.asInstanceOf[FlatZincCompilerResult].maybeNeighbourhood.get

        protected def violation: BooleanValue =
            result.costsOfBestProposal match {
                case violation: BooleanValue => violation
                case costs: PolymorphicListValue => costs.value(0).asInstanceOf[BooleanValue]
            }

        protected def quality(i: Int): AnyValue =
            result.costsOfBestProposal.asInstanceOf[PolymorphicListValue].value(i)

        protected def quality: AnyValue =
            result.quality(1)

        protected def searchVariables: Set[AnyVariable] =
            result.space.searchVariables

        protected def numberOfConstraints[T <: Constraint](implicit classTag: ClassTag[T]): Int =
            result.space.numberOfConstraints(classTag.runtimeClass.isInstance)

        protected def assignment: SearchState =
            result.space.searchState

        protected def warmStartWasPerformed: Boolean =
            result.maybeUserData.get.asInstanceOf[FlatZincCompilerResult].performWarmStart

        protected def searchWasPerformed: Boolean =
            ! result.asInstanceOf[AnnealingResult].roundLogs.isEmpty

    }

    protected implicit def createTask(problemName: String): MiniZincTestTask = task.copy(problemName = problemName)

}
