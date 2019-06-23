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

    private def createTask(space: Space, i: Int, d: IntegerDomain): CumulativeTask =
        new CumulativeTask(
            new IntegerVariable(space.nextVariableId, "s%d".format(i), d),
            new IntegerVariable(space.nextVariableId, "d%d".format(i), d),
            new IntegerVariable(space.nextVariableId, "d%d".format(i), d))

    @Test
    def testSearchVariables {
        val space = new Space(logger, sigint)
        val d = new IntegerRange(Zero, Nine)
        val tasks = (1 to 2).map(createTask(space, _, d))
        val Vector(t1, t2) = tasks
        val ub = new IntegerVariable(space.nextVariableId, "ub", d)
        val costs = new BooleanVariable(space.nextVariableId, "costs", CompleteBooleanDomain)
        space.post(new Cumulative(space.nextConstraintId, null, tasks, ub, costs))
        assertEq(space.searchVariables, Set(t1.s, t1.d, t1.c, t2.s, t2.d, t2.c, ub))
    }

    @Test
    def testTaskMovement {
        val space = new Space(logger, sigint)
        val d = new IntegerRange(Zero, Nine)
        val tasks = (1 to 2).map(createTask(space, _, d))
        val Vector(t1, t2) = tasks
        val ub = new IntegerVariable(space.nextVariableId, "ub", d)
        val costs = new BooleanVariable(space.nextVariableId, "costs", CompleteBooleanDomain)
        // initial conflict: two tasks with same start time exceed capacity
        space
            .post(new Cumulative(space.nextConstraintId, null, tasks, ub, costs))
            .setValue(t1.s, Zero).setValue(t1.d, Three).setValue(t1.c, Two)
            .setValue(t2.s, Zero).setValue(t2.d, Four).setValue(t2.c, Three)
            .setValue(ub, Three)
            .initialize
        val now = space.searchState
        assertEq(now.value(costs), False6)
        if (true) {
            // move t2 to reduce conflict
            val move = new ChangeValue(space.nextMoveId, t2.s, One)
            val after = space.consult(move)
            assertEq(after.value(costs), False4)
            space.commit(move)
            assertEq(now.value(costs), False4)
        }
        if (true) {
            // move t2 to resolve conflict
            val move = new ChangeValue(space.nextMoveId, t2.s, Three)
            val after = space.consult(move)
            assertEq(after.value(costs), True)
            space.commit(move)
            assertEq(now.value(costs), True)
        }
        if (true) {
            // move both tasks in one move
            val move =
                new ChangeValues(
                    space.nextMoveId,
                    List(new ImmutableEffect(t1.s, One), new ImmutableEffect(t2.s, Two)))
            val after = space.consult(move)
            assertEq(after.value(costs), False4)
            space.commit(move)
            assertEq(now.value(costs), False4)
        }
        space.initialize
        assertEq(now.value(costs), False4)
    }

    @Test
    def testTaskResizing {
        val space = new Space(logger, sigint)
        val d = new IntegerRange(Zero, Nine)
        val tasks = (1 to 2).map(createTask(space, _, d))
        val Vector(t1, t2) = tasks
        val ub = new IntegerVariable(space.nextVariableId, "ub", d)
        val costs = new BooleanVariable(space.nextVariableId, "costs", CompleteBooleanDomain)
        // initial conflict: two tasks with same start time exceed capacity
        space
            .post(new Cumulative(space.nextConstraintId, null, tasks, ub, costs))
            .setValue(t1.s, Zero).setValue(t1.d, Three).setValue(t1.c, Two)
            .setValue(t2.s, Zero).setValue(t2.d, Four).setValue(t2.c, Three)
            .setValue(ub, Three)
            .initialize
        val now = space.searchState
        assertEq(now.value(costs), False6)
        if (true) {
            // reduce conflict by reducing the the duration of t1
            val move = new ChangeValue(space.nextMoveId, t1.d, One)
            val after = space.consult(move)
            assertEq(after.value(costs), False2)
            space.commit(move)
            assertEq(now.value(costs), False2)
        }
        if (true) {
            // resolve conflict by setting the duration of t1 to zero
            val move = new ChangeValue(space.nextMoveId, t1.d, Zero)
            val after = space.consult(move)
            assertEq(after.value(costs), True)
            space.commit(move)
            assertEq(now.value(costs), True)
        }
        if (true) {
            // restore duration of t1 and, instead, set its consumption to zero
            val move =
                new ChangeValues(
                    space.nextMoveId,
                    List(new ImmutableEffect(t1.d, Three), new ImmutableEffect(t1.c, Zero)))
            val after = space.consult(move)
            assertEq(after.value(costs), True)
            space.commit(move)
            assertEq(now.value(costs), True)
        }
        if (true) {
            // increase consumption of t2 beyond capacity
            val move = new ChangeValue(space.nextMoveId, t2.c, Four)
            val after = space.consult(move)
            assertEq(after.value(costs), False4)
            space.commit(move)
            assertEq(now.value(costs), False4)
        }
        if (true) {
            // change duration and resource consumption of both tasks in one move
            val move =
                new ChangeValues(
                    space.nextMoveId,
                    List(new ImmutableEffect(t1.d, Four), new ImmutableEffect(t1.c, Two),
                         new ImmutableEffect(t2.d, Five), new ImmutableEffect(t2.c, Three)))
            val after = space.consult(move)
            assertEq(after.value(costs), False8)
            space.commit(move)
            assertEq(now.value(costs), False8)
        }
        space.initialize
        assertEq(now.value(costs), False8)
    }

    @Test
    def testCapacityChanges {
        val space = new Space(logger, sigint)
        val d = new IntegerRange(Zero, Nine)
        val tasks = (1 to 1).map(createTask(space, _, d))
        val Vector(t1) = tasks
        val ub = new IntegerVariable(space.nextVariableId, "ub", d)
        val costs = new BooleanVariable(space.nextVariableId, "costs", CompleteBooleanDomain)
        // initial conflict: consumption exceeds capacity
        space
            .post(new Cumulative(space.nextConstraintId, null, tasks, ub, costs))
            .setValue(t1.s, Zero).setValue(t1.d, Three).setValue(t1.c, Four)
            .setValue(ub, Three)
            .initialize
        val now = space.searchState
        assertEq(now.value(costs), False3)
        if (true) {
            // resolve conflict by increasing capacity
            val move = new ChangeValue(space.nextMoveId, ub, Four)
            val after = space.consult(move)
            assertEq(after.value(costs), True)
            space.commit(move)
            assertEq(now.value(costs), True)
        }
        if (true) {
            // increase consumption to cause conflict
            val move = new ChangeValue(space.nextMoveId, t1.c, Five)
            val after = space.consult(move)
            assertEq(after.value(costs), False3)
            space.commit(move)
            assertEq(now.value(costs), False3)
        }
        space.initialize
        assertEq(now.value(costs), False3)
    }

    @Test
    def testHandlingOfDuplicateVariables {
        val space = new Space(logger, sigint)
        val d = new IntegerRange(Zero, Nine)
        val ub = new IntegerVariable(space.nextVariableId, "ub", d)
        val s2 = new IntegerVariable(space.nextVariableId, "s2", d)
        val d2 = new IntegerVariable(space.nextVariableId, "d2", d)
        val c2 = new IntegerVariable(space.nextVariableId, "c2", d)
        val t1 = new CumulativeTask(ub, ub, ub)
        val t2 = new CumulativeTask(ub, ub, ub)
        val t3 = new CumulativeTask(s2, d2, c2)
        val costs = new BooleanVariable(space.nextVariableId, "costs", CompleteBooleanDomain)
        // baseline: capacity is zero, so t1 and t2 are empty and t3 exceeds capacity
        space
            .post(new Cumulative(space.nextConstraintId, null, Vector(t1, t2, t3), ub, costs))
            .setValue(ub, Zero)
            .setValue(s2, Three).setValue(d2, One).setValue(c2, One)
            .initialize
        val now = space.searchState
        assertEq(now.value(costs), False)
        if (true) {
            // increase capacity and hence duration and consumption of t1 and t2
            val move = new ChangeValue(space.nextMoveId, ub, One)
            val after = space.consult(move)
            assertEq(after.value(costs), False)
            space.commit(move)
            assertEq(now.value(costs), False)
        }
        if (true) {
            // increasing capacity once more makes t1 and t2 overlap t3
            val move = new ChangeValue(space.nextMoveId, ub, Two)
            val after = space.consult(move)
            assertEq(after.value(costs), False5)
            space.commit(move)
            assertEq(now.value(costs), False5)
        }
        space.initialize
        assertEq(now.value(costs), False5)
    }

    @Test
    def testConsultWithoutCommit {
        val space = new Space(logger, sigint)
        val d = new IntegerRange(Zero, Nine)
        val tasks = (1 to 1).map(createTask(space, _, d))
        val Vector(t1) = tasks
        val ub = new IntegerVariable(space.nextVariableId, "ub", d)
        val costs = new BooleanVariable(space.nextVariableId, "costs", CompleteBooleanDomain)
        // initial conflict: consumption is too high
        space
            .post(new Cumulative(space.nextConstraintId, null, tasks, ub, costs))
            .setValue(t1.s, Zero).setValue(t1.d, Three).setValue(t1.c, Two)
            .setValue(ub, One)
            .initialize
        val now = space.searchState
        assertEq(now.value(costs), False3)
        if (true) {
            // resolve conflict by reducing consumption
            val move = new ChangeValue(space.nextMoveId, t1.c, One)
            val after = space.consult(move)
            assertEq(after.value(costs), True)
        }
        if (true) {
            // resolve conflict by increasing capacity
            val move = new ChangeValue(space.nextMoveId, ub, Two)
            val after = space.consult(move)
            assertEq(after.value(costs), True)
        }
        space.initialize
        assertEq(now.value(costs), False3)
    }

    @Test
    def testComplexMoves {
        val space = new Space(logger, sigint)
        val d = new IntegerRange(Zero, Nine)
        val tasks = (1 to 2).map(createTask(space, _, d))
        val Vector(t1, t2) = tasks
        val ub = new IntegerVariable(space.nextVariableId, "ub", d)
        val costs = new BooleanVariable(space.nextVariableId, "costs", CompleteBooleanDomain)
        // no initial conflict
        space
            .post(new Cumulative(space.nextConstraintId, null, tasks, ub, costs))
            .setValue(t1.s, Zero).setValue(t1.d, Three).setValue(t1.c, One)
            .setValue(t2.s, Three).setValue(t2.d, Four).setValue(t2.c, Three)
            .setValue(ub, Three)
            .initialize
        val now = space.searchState
        assertEq(now.value(costs), True)
        if (true) {
            // move and resize t1 in one move
            val move =
                new ChangeValues(
                    space.nextMoveId,
                    List(new ImmutableEffect(t1.s, Two), new ImmutableEffect(t1.d, Seven), new ImmutableEffect(t1.c, Two)))
            val after = space.consult(move)
            assertEq(after.value(costs), False8)
            space.commit(move)
            assertEq(now.value(costs), False8)
        }
        if (true) {
            // change consumption of both tasks and capacity in one move
            val move =
                new ChangeValues(
                    space.nextMoveId,
                    List(new ImmutableEffect(t1.d, Six), new ImmutableEffect(t1.c, Four),
                         new ImmutableEffect(t2.s, Seven), new ImmutableEffect(t2.c, Two),
                         new ImmutableEffect(ub, Four)))
            val after = space.consult(move)
            assertEq(after.value(costs), False2)
            space.commit(move)
            assertEq(now.value(costs), False2)
        }
        space.initialize
        assertEq(now.value(costs), False2)
    }

}
