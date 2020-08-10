package yuck.constraints.test

import scala.language.implicitConversions

import yuck.core._
import yuck.util.logging.LazyLogger
import yuck.util.testing.YuckAssert

/**
 * @author Michael Marte
 *
 */
trait StandardConstraintTestTooling[ResultValue <: AnyValue] extends YuckAssert {

    protected val logger: LazyLogger

    protected abstract class TestStep
    protected case class Initialize(comment: String, expectedResult: ResultValue, effects: List[AnyMoveEffect]) extends TestStep
    protected object Initialize {
        def apply(comment: String, expectedResult: ResultValue, effects: AnyMoveEffect*): Initialize =
            Initialize(comment, expectedResult, effects.toList)
    }
    protected case class Consult(comment: String, expectedResult: ResultValue, effects: List[AnyMoveEffect]) extends TestStep
    protected object Consult {
        def apply(comment: String, expectedResult: ResultValue, effects: AnyMoveEffect*): Consult =
            Consult(comment, expectedResult, effects.toList)
    }
    protected case class ConsultAndCommit(comment: String, expectedResult: ResultValue, effects: List[AnyMoveEffect]) extends TestStep
    protected object ConsultAndCommit {
        def apply(comment: String, expectedResult: ResultValue, effects: AnyMoveEffect*): ConsultAndCommit =
            ConsultAndCommit(comment, expectedResult, effects.toList)
    }
    protected case class TestScenario(space: Space, result: Variable[ResultValue], steps: TestStep*)

    protected implicit def createMoveEffect[Value <: AnyValue](assignment: (Variable[Value], Value)) =
        new ImmutableMoveEffect[Value](assignment._1, assignment._2)

    protected def runScenario(scenario: TestScenario): Unit = {
        val space = scenario.space
        val result = scenario.result
        val steps = scenario.steps
        val now = space.searchState
        var maybeFinalResult: Option[ResultValue] = None
        for (step <- steps) step match {
            case Initialize(comment, expectedResult, effects) =>
                logger.log(comment)
                for (effect <- effects) {
                    effect.affect(space)
                }
                space.initialize
                assertEq(now.value(result), expectedResult)
                maybeFinalResult = Some(expectedResult)
            case Consult(comment, expectedResult, effects) =>
                logger.log(comment)
                val move = new ChangeAnyValues(space.nextMoveId, effects)
                val after = space.consult(move)
                assertEq(after.value(result), expectedResult)
            case ConsultAndCommit(comment, expectedResult, effects) =>
                logger.log(comment)
                val move = new ChangeAnyValues(space.nextMoveId, effects)
                val after = space.consult(move)
                assertEq(after.value(result), expectedResult)
                space.commit(move)
                assertEq(now.value(result), expectedResult)
                maybeFinalResult = Some(expectedResult)
        }
        if (maybeFinalResult.isDefined) {
            space.initialize
            assertEq(now.value(result), maybeFinalResult.get)
        }
    }

}
