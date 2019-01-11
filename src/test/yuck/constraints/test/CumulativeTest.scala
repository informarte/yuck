package yuck.constraints.test

import org.junit._

import scala.collection._

import yuck.constraints._
import yuck.core._
import yuck.util.testing.UnitTest

/**
 * @author Michael Marte
 *
 */
@Test
@FixMethodOrder(runners.MethodSorters.NAME_ASCENDING)
final class CumulativeTest extends UnitTest {

    @Test
    def testCumulative {
        val space = new Space(logger)
        val d = new IntegerRange(Zero, Nine)
        val s0 = new IntegerVariable(space.nextVariableId, "s0", d)
        val d0 = new IntegerVariable(space.nextVariableId, "d0", d)
        val c0 = new IntegerVariable(space.nextVariableId, "c0", d)
        val s1 = new IntegerVariable(space.nextVariableId, "s1", d)
        val d1 = new IntegerVariable(space.nextVariableId, "d1", d)
        val c1 = new IntegerVariable(space.nextVariableId, "c1", d)
        val t0 = new CumulativeTask(s0, d0, c0)
        val t1 = new CumulativeTask(s1, d1, c1)
        val ub = new IntegerVariable(space.nextVariableId, "ub", d)
        val costs = new BooleanVariable(space.nextVariableId, "costs", CompleteBooleanDomain)
        val c = new Cumulative(space.nextConstraintId, null, List(t0, t1), ub, costs)
        // initial conflict: two tasks with same start time and duration
        space
            .post(c)
            .setValue(s0, Zero)
            .setValue(d0, Three)
            .setValue(c0, Two)
            .setValue(s1, Zero)
            .setValue(d1, Three)
            .setValue(c1, Two)
            .setValue(ub, Three)
            .initialize
        assertEq(space.searchVariables, Set(s0, d0, c0, s1, d1, c1, ub))
        val now = space.searchState
        assertEq(now.value(costs), False3)
        if (true) {
            // move second task to resolve conflict
            val move = new ChangeValue(space.nextMoveId, s1, Three)
            val after = space.consult(move)
            assertEq(after.value(s1), Three)
            assertEq(after.value(costs), True)
            space.commit(move)
            assertEq(now.value(s1), Three)
            assertEq(now.value(costs), True)
        }
        if (true) {
            // make first task longer to create overlap
            val move = new ChangeValue(space.nextMoveId, d0, Four)
            val after = space.consult(move)
            assertEq(after.value(d0), Four)
            assertEq(after.value(costs), False)
            space.commit(move)
            assertEq(now.value(d0), Four)
            assertEq(now.value(costs), False)
        }
        if (true) {
            // resolve conflict by increasing resource supply
            val move = new ChangeValue(space.nextMoveId, ub, Four)
            val after = space.consult(move)
            assertEq(after.value(ub), Four)
            assertEq(after.value(costs), True)
            space.commit(move)
            assertEq(now.value(ub), Four)
            assertEq(now.value(costs), True)
        }
        if (true) {
            // change duration and resource consumption in one move
            val move =
                new ChangeValues(
                    space.nextMoveId,
                    List(new ImmutableEffect(d0, Three), new ImmutableEffect(c0, Five)))
            val after = space.consult(move)
            assertEq(after.value(d0), Three)
            assertEq(after.value(c0), Five)
            assertEq(after.value(costs), False3)
            space.commit(move)
            assertEq(now.value(d0), Three)
            assertEq(now.value(c0), Five)
            assertEq(now.value(costs), False3)
        }
        space.initialize
        assertEq(now.value(costs), False3)
    }

}
