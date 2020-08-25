package yuck.constraints.test

import scala.language.implicitConversions

import yuck.core._
import yuck.util.logging.LazyLogger
import yuck.util.testing.YuckAssert

/**
 * @author Michael Marte
 *
 */
trait CostComputationTestTooling[ResultValue <: AnyValue] extends YuckAssert {

    protected val logger: LazyLogger

    protected abstract class CostComputationTestStep
    protected case class Initialize(comment: String, expectedResult: ResultValue, effects: List[AnyMoveEffect]) extends CostComputationTestStep
    protected object Initialize {
        def apply(comment: String, expectedResult: ResultValue, effects: AnyMoveEffect*): Initialize =
            Initialize(comment, expectedResult, effects.toList)
    }
    protected case class Consult(comment: String, expectedResult: ResultValue, effects: List[AnyMoveEffect]) extends CostComputationTestStep
    protected object Consult {
        def apply(comment: String, expectedResult: ResultValue, effects: AnyMoveEffect*): Consult =
            Consult(comment, expectedResult, effects.toList)
    }
    protected case class ConsultAndCommit(comment: String, expectedResult: ResultValue, effects: List[AnyMoveEffect]) extends CostComputationTestStep
    protected object ConsultAndCommit {
        def apply(comment: String, expectedResult: ResultValue, effects: AnyMoveEffect*): ConsultAndCommit =
            ConsultAndCommit(comment, expectedResult, effects.toList)
    }
    protected case class CostComputationTestScenario(space: Space, result: Variable[ResultValue], steps: CostComputationTestStep*)

    protected implicit def createMoveEffect[Value <: AnyValue](assignment: (Variable[Value], Value)) =
        new ImmutableMoveEffect[Value](assignment._1, assignment._2)

    protected def runScenario(scenario: CostComputationTestScenario): Unit = {
        val space = scenario.space
        val result = scenario.result
        val steps = scenario.steps
        val now = space.searchState
        for (step <- steps) step match {
            case Initialize(comment, expectedResult, effects) =>
                logger.log(comment)
                for (effect <- effects) {
                    effect.affect(space)
                }
                space.initialize
                assertEq(now.value(result), expectedResult)
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
        }
    }

}
