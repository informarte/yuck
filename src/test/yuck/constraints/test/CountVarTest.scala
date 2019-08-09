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
final class CountVarTest extends UnitTest {

    @Test
    def testCountVar: Unit = {
        val space = new Space(logger)
        val d = new IntegerRange(Zero, Nine)
        val s = space.createVariable("s", d)
        val t = space.createVariable("t", d)
        val u = space.createVariable("u", d)
        val what = space.createVariable("what", d)
        val n = new IntegerVariable(space.nextVariableId, "n", NonNegativeIntegerRange)
        val c = new CountVar(space.nextConstraintId, null, List(s, t, u), what, n)
        space
            .post(c)
            .setValue(s, One)
            .setValue(t, One)
            .setValue(u, One)
            .setValue(what, One)
            .initialize
        assertEq(space.searchVariables, Set(s, t, u, what))
        val now = space.searchState
        assertEq(now.value(n), Three)
        if (true) {
            val move = new ChangeValue(space.nextMoveId, s, Two)
            val after = space.consult(move)
            assertEq(after.value(n), Two)
            space.commit(move)
            assertEq(now.value(n), Two)
        }
        if (true) {
            val move = new ChangeValue(space.nextMoveId, what, Two)
            val after = space.consult(move)
            assertEq(after.value(n), One)
            space.commit(move)
            assertEq(now.value(n), One)
        }
        space.initialize
        assertEq(now.value(n), One)
    }

}
