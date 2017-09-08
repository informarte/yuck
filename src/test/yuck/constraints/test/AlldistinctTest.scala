package yuck.constraints.test

import org.junit._

import scala.collection._

import yuck.annealing._
import yuck.constraints._
import yuck.core._
import yuck.util.testing.UnitTest

/**
 * @author Michael Marte
 *
 */
@Test
@FixMethodOrder(runners.MethodSorters.NAME_ASCENDING)
final class AlldistinctTest extends UnitTest {

    @Test
    def testAlldistinct {
        val space = new Space(logger)
        val d = new IntegerDomain(Zero, Nine)
        val s = space.createVariable("s", d)
        val t = space.createVariable("t", d)
        val u = space.createVariable("u", d)
        val costs = space.createVariable("costs", NonNegativeIntegerDomain)
        val c = new Alldistinct(space.constraintIdFactory.nextId, null, Vector(s, t, u), costs)
        space
            .post(c)
            .setValue(s, One)
            .setValue(t, One)
            .setValue(u, One)
            .initialize
        assertEq(space.searchVariables, Set(s, t, u))
        val now = space.searchState
        assertEq(now.value(costs), Two)
        if (true) {
            val move = new ChangeValue(space.moveIdFactory.nextId, s, Two)
            val after = space.consult(move)
            assertEq(now.value(s), One)
            assertEq(now.value(costs), Two)
            assertEq(after.value(s), Two)
            assertEq(after.value(costs), One)
            space.commit(move)
            assertEq(now.value(s), Two)
            assertEq(now.value(costs), One)
        }
        space.initialize
        assertEq(now.value(costs), One)
    }

    @Test
    def testAlldistinctWithAVariableOccuringTwice {
        val space = new Space(logger)
        val d = new IntegerDomain(Zero, Nine)
        val s = space.createVariable("s", d)
        val t = space.createVariable("t", d)
        val costs = space.createVariable("costs", NonNegativeIntegerDomain)
        val c = new Alldistinct(space.constraintIdFactory.nextId, null, Vector(s, t, t), costs)
        space
            .post(c)
            .setValue(s, One)
            .setValue(t, One)
            .initialize
        assertEq(space.searchVariables, Set(s, t))
        val now = space.searchState
        assertEq(now.value(costs), Two)
        if (true) {
            val move = new ChangeValue(space.moveIdFactory.nextId, s, Two)
            val after = space.consult(move)
            assertEq(now.value(s), One)
            assertEq(now.value(costs), Two)
            assertEq(after.value(s), Two)
            assertEq(after.value(costs), One)
            space.commit(move)
            assertEq(now.value(s), Two)
            assertEq(now.value(costs), One)
        }
        if (true) {
            val move = new ChangeValue(space.moveIdFactory.nextId, t, Two)
            val after = space.consult(move)
            assertEq(now.value(t), One)
            assertEq(now.value(costs), One)
            assertEq(after.value(t), Two)
            assertEq(after.value(costs), Two)
            space.commit(move)
            assertEq(now.value(t), Two)
            assertEq(now.value(costs), Two)
        }
        space.initialize
        assertEq(now.value(costs), Two)
    }

    @Test
    def testAlldistinctWithImplicitSolving {
        val space = new Space(logger)
        val d = new IntegerDomain(Zero, Two)
        val s = space.createVariable("s", d)
        val t = space.createVariable("t", d)
        val u = space.createVariable("u", d)
        val costs = space.createVariable("costs", NonNegativeIntegerDomain)
        val c = new Alldistinct(space.constraintIdFactory.nextId, null, Vector(s, t, u), costs)
        space
            .post(c)
            .setValue(s, One)
            .setValue(t, One)
            .setValue(u, One)
            .initialize
        val now = space.searchState
        assertEq(now.value(costs), Two)
        val maybeNeighbourhood =
            c.prepareForImplicitSolving(
                space, new JavaRandomGenerator, DEFAULT_MOVE_SIZE_DISTRIBUTION, _ => None, 0, new SettableSigint)
        assert(maybeNeighbourhood.isDefined)
        val neighbourhood = maybeNeighbourhood.get
        assertNe(now.value(s), now.value(t))
        assertNe(now.value(s), now.value(u))
        assertNe(now.value(t), now.value(u))
        assertEq(now.value(costs), Zero)
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
    def testAlldistinctExceptZero {
        val space = new Space(logger)
        val d = new IntegerDomain(Zero, Nine)
        val s = space.createVariable("s", d)
        val t = space.createVariable("t", d)
        val u = space.createVariable("u", d)
        val costs = space.createVariable("costs", NonNegativeIntegerDomain)
        val c = new AlldistinctExceptZero(space.constraintIdFactory.nextId, null, List(s, t, u), costs)
        space
            .post(c)
            .setValue(s, One)
            .setValue(t, One)
            .setValue(u, One)
            .initialize
        assertEq(space.searchVariables, Set(s, t, u))
        val now = space.searchState
        assertEq(now.value(costs), Two)
        if (true) {
            val move = new ChangeValue(space.moveIdFactory.nextId, s, Zero)
            val after = space.consult(move)
            assertEq(now.value(s), One)
            assertEq(now.value(costs), Two)
            assertEq(after.value(s), Zero)
            assertEq(after.value(costs), One)
            space.commit(move)
            assertEq(now.value(s), Zero)
            assertEq(now.value(costs), One)
        }
        if (true) {
            val move = new ChangeValue(space.moveIdFactory.nextId, t, Zero)
            val after = space.consult(move)
            assertEq(now.value(t), One)
            assertEq(now.value(costs), One)
            assertEq(after.value(t), Zero)
            assertEq(after.value(costs), Zero)
            space.commit(move)
            assertEq(now.value(t), Zero)
            assertEq(now.value(costs), Zero)
        }
        if (true) {
            val move = new ChangeValue(space.moveIdFactory.nextId, u, Zero)
            val after = space.consult(move)
            assertEq(now.value(u), One)
            assertEq(now.value(costs), Zero)
            assertEq(after.value(u), Zero)
            assertEq(after.value(costs), Zero)
            space.commit(move)
            assertEq(now.value(u), Zero)
            assertEq(now.value(costs), Zero)
        }
        space.initialize
        assertEq(now.value(costs), Zero)
    }

    @Test
    def testAlldistinctExceptZeroWithAVariableOccuringTwice {
        val space = new Space(logger)
        val d = new IntegerDomain(Zero, Nine)
        val s = space.createVariable("s", d)
        val t = space.createVariable("t", d)
        val costs = space.createVariable("costs", NonNegativeIntegerDomain)
        val c = new AlldistinctExceptZero(space.constraintIdFactory.nextId, null, List(s, t, t), costs)
        space
            .post(c)
            .setValue(s, One)
            .setValue(t, One)
            .initialize
        assertEq(space.searchVariables, Set(s, t))
        val now = space.searchState
        assertEq(now.value(costs), Two)
        if (true) {
            val move = new ChangeValue(space.moveIdFactory.nextId, s, Zero)
            val after = space.consult(move)
            assertEq(now.value(s), One)
            assertEq(now.value(costs), Two)
            assertEq(after.value(s), Zero)
            assertEq(after.value(costs), One)
            space.commit(move)
            assertEq(now.value(s), Zero)
            assertEq(now.value(costs), One)
        }
        if (true) {
            val move = new ChangeValue(space.moveIdFactory.nextId, t, Zero)
            val after = space.consult(move)
            assertEq(now.value(t), One)
            assertEq(now.value(costs), One)
            assertEq(after.value(t), Zero)
            assertEq(after.value(costs), Zero)
            space.commit(move)
            assertEq(now.value(t), Zero)
            assertEq(now.value(costs), Zero)
        }
        if (true) {
            val move = new ChangeValue(space.moveIdFactory.nextId, s, One)
            val after = space.consult(move)
            assertEq(now.value(s), Zero)
            assertEq(now.value(costs), Zero)
            assertEq(after.value(s), One)
            assertEq(after.value(costs), Zero)
            space.commit(move)
            assertEq(now.value(s), One)
            assertEq(now.value(costs), Zero)
        }
        if (true) {
            val move = new ChangeValue(space.moveIdFactory.nextId, t, Two)
            val after = space.consult(move)
            assertEq(now.value(t), Zero)
            assertEq(now.value(costs), Zero)
            assertEq(after.value(t), Two)
            assertEq(after.value(costs), One)
            space.commit(move)
            assertEq(now.value(t), Two)
            assertEq(now.value(costs), One)
        }
        space.initialize
        assertEq(now.value(costs), One)
    }

}
