package yuck.constraints.test

import org.junit._
import scala.collection._

import yuck.annealing._
import yuck.constraints._
import yuck.core._
import yuck.util.arm.SettableSigint
import yuck.util.testing.UnitTest

/**
 * @author Michael Marte
 *
 */
@Test
@FixMethodOrder(runners.MethodSorters.NAME_ASCENDING)
final class AlldistinctTest extends UnitTest {

    @Test
    def testAlldistinct: Unit = {
        val space = new Space(logger)
        val d = new IntegerRange(Zero, Nine)
        val s = space.createVariable("s", d)
        val t = space.createVariable("t", d)
        val u = space.createVariable("u", d)
        val costs = new BooleanVariable(space.nextVariableId, "costs", CompleteBooleanDomain)
        val c = new Alldistinct(space.nextConstraintId, null, Vector(s, t, u), costs)
        space
            .post(c)
            .setValue(s, One)
            .setValue(t, One)
            .setValue(u, One)
            .initialize
        assertEq(space.searchVariables, Set(s, t, u))
        val now = space.searchState
        assertEq(now.value(costs), False2)
        if (true) {
            val move = new ChangeValue(space.nextMoveId, s, Two)
            val after = space.consult(move)
            assertEq(after.value(costs), False)
            space.commit(move)
            assertEq(now.value(costs), False)
        }
        space.initialize
        assertEq(now.value(costs), False)
    }

    @Test
    def testAlldistinctWithAVariableOccuringTwice: Unit = {
        val space = new Space(logger)
        val d = new IntegerRange(Zero, Nine)
        val s = space.createVariable("s", d)
        val t = space.createVariable("t", d)
        val costs = new BooleanVariable(space.nextVariableId, "costs", CompleteBooleanDomain)
        val c = new Alldistinct(space.nextConstraintId, null, Vector(s, t, t), costs)
        space
            .post(c)
            .setValue(s, One)
            .setValue(t, One)
            .initialize
        assertEq(space.searchVariables, Set(s, t))
        val now = space.searchState
        assertEq(now.value(costs), False2)
        if (true) {
            val move = new ChangeValue(space.nextMoveId, s, Two)
            val after = space.consult(move)
            assertEq(after.value(costs), False)
            space.commit(move)
            assertEq(now.value(costs), False)
        }
        if (true) {
            val move = new ChangeValue(space.nextMoveId, t, Two)
            val after = space.consult(move)
            assertEq(after.value(costs), False2)
            space.commit(move)
            assertEq(now.value(costs), False2)
        }
        space.initialize
        assertEq(now.value(costs), False2)
    }

    @Test
    def testAlldistinctWithImplicitSolving: Unit = {
        val space = new Space(logger)
        val d = new IntegerRange(Zero, Two)
        val s = space.createVariable("s", d)
        val t = space.createVariable("t", d)
        val u = space.createVariable("u", d)
        val costs = new BooleanVariable(space.nextVariableId, "costs", CompleteBooleanDomain)
        val c = new Alldistinct(space.nextConstraintId, null, Vector(s, t, u), costs)
        space
            .post(c)
            .setValue(s, One)
            .setValue(t, One)
            .setValue(u, One)
            .initialize
        val now = space.searchState
        assertEq(now.value(costs), False2)
        val maybeNeighbourhood =
            c.prepareForImplicitSolving(
                space, new JavaRandomGenerator, DefaultMoveSizeDistribution, _ => None, None, new SettableSigint)
        assert(maybeNeighbourhood.isDefined)
        val neighbourhood = maybeNeighbourhood.get
        assertNe(now.value(s), now.value(t))
        assertNe(now.value(s), now.value(u))
        assertNe(now.value(t), now.value(u))
        assertEq(now.value(costs), True)
        space.initialize
        for (i <- 1 to 10000) {
            val move = neighbourhood.nextMove
            val after = space.consult(move)
            assert(List(s, t, u).exists(x => now.value(x) != after.value(x)))
            assertNe(after.value(s), after.value(t))
            assertNe(after.value(s), after.value(u))
            assertNe(after.value(t), after.value(u))
        }
    }

    @Test
    def testAlldistinctExceptZero: Unit = {
        val space = new Space(logger)
        val d = new IntegerRange(Zero, Nine)
        val s = new IntegerVariable(space.nextVariableId, "s", d)
        val t = new IntegerVariable(space.nextVariableId, "t", d)
        val u = new IntegerVariable(space.nextVariableId, "u", d)
        val costs = new BooleanVariable(space.nextVariableId, "costs", CompleteBooleanDomain)
        val c = new AlldistinctExceptZero(space.nextConstraintId, null, List(s, t, u), costs)
        space
            .post(c)
            .setValue(s, One)
            .setValue(t, One)
            .setValue(u, One)
            .initialize
        assertEq(space.searchVariables, Set(s, t, u))
        val now = space.searchState
        assertEq(now.value(costs), False2)
        if (true) {
            val move = new ChangeValue(space.nextMoveId, s, Zero)
            val after = space.consult(move)
            assertEq(after.value(costs), False)
            space.commit(move)
            assertEq(now.value(costs), False)
        }
        if (true) {
            val move = new ChangeValue(space.nextMoveId, t, Zero)
            val after = space.consult(move)
            assertEq(after.value(costs), True)
            space.commit(move)
            assertEq(now.value(costs), True)
        }
        if (true) {
            val move = new ChangeValue(space.nextMoveId, u, Zero)
            val after = space.consult(move)
            assertEq(after.value(costs), True)
            space.commit(move)
            assertEq(now.value(costs), True)
        }
        space.initialize
        assertEq(now.value(costs), True)
    }

    @Test
    def testAlldistinctExceptZeroWithAVariableOccuringTwice: Unit = {
        val space = new Space(logger)
        val d = new IntegerRange(Zero, Nine)
        val s = new IntegerVariable(space.nextVariableId, "s", d)
        val t = new IntegerVariable(space.nextVariableId, "t", d)
        val costs = new BooleanVariable(space.nextVariableId, "costs", CompleteBooleanDomain)
        val c = new AlldistinctExceptZero(space.nextConstraintId, null, List(s, t, t), costs)
        space
            .post(c)
            .setValue(s, One)
            .setValue(t, One)
            .initialize
        assertEq(space.searchVariables, Set(s, t))
        val now = space.searchState
        assertEq(now.value(costs), False2)
        if (true) {
            val move = new ChangeValue(space.nextMoveId, s, Zero)
            val after = space.consult(move)
            assertEq(after.value(costs), False)
            space.commit(move)
            assertEq(now.value(costs), False)
        }
        if (true) {
            val move = new ChangeValue(space.nextMoveId, t, Zero)
            val after = space.consult(move)
            assertEq(after.value(costs), True)
            space.commit(move)
            assertEq(now.value(costs), True)
        }
        if (true) {
            val move = new ChangeValue(space.nextMoveId, s, One)
            val after = space.consult(move)
            assertEq(after.value(costs), True)
            space.commit(move)
            assertEq(now.value(costs), True)
        }
        if (true) {
            val move = new ChangeValue(space.nextMoveId, t, Two)
            val after = space.consult(move)
            assertEq(after.value(costs), False)
            space.commit(move)
            assertEq(now.value(costs), False)
        }
        space.initialize
        assertEq(now.value(costs), False)
    }

}
