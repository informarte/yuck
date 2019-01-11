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
final class MinimumTest extends UnitTest {

    @Test
    def testMinimum {
        val space = new Space(logger)
        val d = new IntegerRange(Zero, Nine)
        val s = new IntegerVariable(space.nextVariableId, "s", d)
        val t = new IntegerVariable(space.nextVariableId, "t", d)
        val u = new IntegerVariable(space.nextVariableId, "u", d)
        val min = new IntegerVariable(space.nextVariableId, "costs", CompleteIntegerRange)
        val c = new Minimum(space.nextConstraintId, null, List(s, t, u), min)
        space
            .post(c)
            .setValue(s, One)
            .setValue(t, Two)
            .setValue(u, Three)
            .initialize
        assertEq(space.searchVariables, Set(s, t, u))
        val now = space.searchState
        assertEq(now.value(min), One)
        if (true) {
            val move = new ChangeValue(space.nextMoveId, s, Two)
            val after = space.consult(move)
            assertEq(after.value(s), Two)
            assertEq(after.value(min), Two)
            space.commit(move)
            assertEq(now.value(s), Two)
            assertEq(now.value(min), Two)
        }
        if (true) {
            val move = new ChangeValue(space.nextMoveId, u, One)
            val after = space.consult(move)
            assertEq(after.value(u), One)
            assertEq(after.value(min), One)
            space.commit(move)
            assertEq(now.value(u), One)
            assertEq(now.value(min), One)
        }
        space.initialize
        assertEq(now.value(min), One)
    }

}
