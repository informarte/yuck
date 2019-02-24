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
    def testConsultAndCommit {
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
        // initial conflict: two tasks with same start time and duration
        space
            .post(new Cumulative(space.nextConstraintId, null, List(t0, t1), ub, costs))
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
        if (true) {
            // move both tasks in one move
            val move =
                new ChangeValues(
                    space.nextMoveId,
                    List(new ImmutableEffect(s0, One), new ImmutableEffect(s1, Two)))
            val after = space.consult(move)
            assertEq(after.value(s0), One)
            assertEq(after.value(s1), Two)
            assertEq(after.value(costs), False7)
            space.commit(move)
            assertEq(now.value(s0), One)
            assertEq(now.value(s1), Two)
            assertEq(now.value(costs), False7)
        }
        if (true) {
            // reduce conflict by increasing resource capacity and lowering first task's consumption
            val move =
                new ChangeValues(
                    space.nextMoveId,
                    List(new ImmutableEffect(ub, Five), new ImmutableEffect(c0, Four)))
            val after = space.consult(move)
            assertEq(after.value(ub), Five)
            assertEq(after.value(c0), Four)
            assertEq(after.value(costs), False2)
            space.commit(move)
            assertEq(now.value(ub), Five)
            assertEq(now.value(c0), Four)
            assertEq(now.value(costs), False2)
        }
        space.initialize
        assertEq(now.value(costs), False2)
    }

    @Test
    def testConsultAndCommitWithDuplicateVariables {
        val space = new Space(logger)
        val d = new IntegerRange(Zero, Nine)
        val ub = new IntegerVariable(space.nextVariableId, "ub", d)
        val s2 = new IntegerVariable(space.nextVariableId, "s2", d)
        val d2 = new IntegerVariable(space.nextVariableId, "d2", d)
        val c2 = new IntegerVariable(space.nextVariableId, "c2", d)
        val t0 = new CumulativeTask(ub, ub, ub)
        val t1 = new CumulativeTask(ub, ub, ub)
        val t2 = new CumulativeTask(s2, d2, c2)
        val costs = new BooleanVariable(space.nextVariableId, "costs", CompleteBooleanDomain)
        // baseline: supply is zero, so t0 and t1 are empty and t2 exceeds supply
        space
            .post(new Cumulative(space.nextConstraintId, null, List(t0, t1, t2), ub, costs))
            .setValue(ub, Zero)
            .setValue(ub, Zero)
            .setValue(s2, Three)
            .setValue(d2, One)
            .setValue(c2, One)
            .initialize
        assertEq(space.searchVariables, Set(ub, s2, d2, c2))
        val now = space.searchState
        assertEq(now.value(costs), False)
        if (true) {
            // increase supply and hence duration and consumption of t0 and t1
            val move = new ChangeValue(space.nextMoveId, ub, One)
            val after = space.consult(move)
            assertEq(after.value(ub), One)
            assertEq(after.value(costs), False)
            space.commit(move)
            assertEq(now.value(ub), One)
            assertEq(now.value(costs), False)
        }
        if (true) {
            // increasing supply once more makes t0 and t1 overlap t2
            val move = new ChangeValue(space.nextMoveId, ub, Two)
            val after = space.consult(move)
            assertEq(after.value(ub), Two)
            assertEq(after.value(costs), False5)
            space.commit(move)
            assertEq(now.value(ub), Two)
            assertEq(now.value(costs), False5)
        }
        space.initialize
        assertEq(now.value(costs), False5)
    }

}
