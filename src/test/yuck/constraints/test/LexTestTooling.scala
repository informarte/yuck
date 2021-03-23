package yuck.constraints.test

import yuck.constraints.{LeRelation, LexLess, LexLessEq, LtRelation, OrderingRelation}
import yuck.core._
import yuck.test.util.YuckAssert

/**
 * @author Michael Marte
 *
 */
trait LexTestTooling [Value <: OrderedValue[Value]] extends YuckAssert {

    protected val valueTraits: OrderedValueTraits[Value]
    protected val space: Space

    protected trait TestVar
    protected case class X(i: Int /* 1-based */) extends TestVar
    protected case class Y(y: Int /* 1-based */) extends TestVar
    protected case class TestStep(expectedResult: BooleanValue, assignments: (TestVar, Value)*)
    protected case class TestScenario(relation: OrderingRelation, n: Int, m: Int, steps: TestStep*)

    protected def runScenario(scenario: TestScenario): Unit = {
        def createVariable(name: String) = valueTraits.createVariable(space, name, valueTraits.completeDomain)
        val xs = for (i <- 1 to scenario.n) yield createVariable("x%d".format(i))
        val ys = for (i <- 1 to scenario.m) yield createVariable("y%d".format(i))
        def fromTestVar(testVar: TestVar) = testVar match {
            case X(i) => xs(i - 1)
            case Y(i) => ys(i - 1)
        }
        val costs = new BooleanVariable(space.nextVariableId, "costs", CompleteBooleanDomain)
        scenario.relation match {
            case LtRelation => space.post(new LexLess[Value](space.nextConstraintId, null, xs, ys, costs))
            case LeRelation => space.post(new LexLessEq[Value](space.nextConstraintId, null, xs, ys, costs))
        }
        assertEq(space.searchVariables, (xs ++ ys).toSet)
        for ((x, a) <- scenario.steps.head.assignments) {
            space.setValue(fromTestVar(x), a)
        }
        val now = space.searchState
        space.initialize()
        assertEq(now.value(costs), scenario.steps.head.expectedResult)
        for (step <- scenario.steps.tail) {
            val effects = for ((x, a) <- step.assignments) yield new ImmutableMoveEffect(fromTestVar(x), a)
            val move = new ChangeValues(space.nextMoveId, effects)
            val after = space.consult(move)
            assertEq(after.value(costs), step.expectedResult)
            space.commit(move)
            assertEq(now.value(costs), step.expectedResult)
        }
        space.initialize()
        assertEq(now.value(costs), scenario.steps.last.expectedResult)
    }

}
