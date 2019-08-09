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
final class MaximumTest extends UnitTest {

    @Test
    def testMaximum: Unit = {
        val space = new Space(logger)
        val d = new IntegerRange(Zero, Nine)
        val s = new IntegerVariable(space.nextVariableId, "s", d)
        val t = new IntegerVariable(space.nextVariableId, "t", d)
        val u = new IntegerVariable(space.nextVariableId, "u", d)
        val max = new IntegerVariable(space.nextVariableId, "costs", CompleteIntegerRange)
        val c = new Maximum(space.nextConstraintId, null, List(s, t, u), max)
        space
            .post(c)
            .setValue(s, One)
            .setValue(t, Two)
            .setValue(u, Three)
            .initialize
        assertEq(space.searchVariables, Set(s, t, u))
        val now = space.searchState
        assertEq(now.value(max), Three)
        if (true) {
            val move = new ChangeValue(space.nextMoveId, u, Two)
            val after = space.consult(move)
            assertEq(after.value(max), Two)
            space.commit(move)
            assertEq(now.value(max), Two)
        }
        if (true) {
            val move = new ChangeValue(space.nextMoveId, s, Three)
            val after = space.consult(move)
            assertEq(after.value(max), Three)
            space.commit(move)
            assertEq(now.value(max), Three)
        }
        space.initialize
        assertEq(now.value(max), Three)
    }

}
