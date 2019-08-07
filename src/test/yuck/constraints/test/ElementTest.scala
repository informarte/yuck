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
final class ElementTest extends UnitTest {

    @Test
    def testElement: Unit = {
        val space = new Space(logger, sigint)
        val d = new IntegerRange(Zero, Nine)
        val s = space.createVariable("s", d)
        val t = space.createVariable("t", d)
        val u = space.createVariable("u", d)
        val xs = immutable.IndexedSeq(s, t, u)
        val i = new IntegerVariable(space.nextVariableId, "i", d)
        val y = space.createVariable("y", NonNegativeIntegerRange)
        val c = new Element(space.nextConstraintId, null, xs, i, y, 0)
        space
            .post(c)
            .setValue(s, One)
            .setValue(t, Two)
            .setValue(u, Three)
            .setValue(i, Zero)
            .initialize
        assertEq(space.searchVariables, Set(s, t, u, i))
        val now = space.searchState
        assertEq(now.value(y), One)
        if (true) {
            val move = new ChangeValue(space.nextMoveId, s, Zero)
            val after = space.consult(move)
            assertEq(after.value(y), Zero)
            space.commit(move)
            assertEq(now.value(y), Zero)
        }
        if (true) {
            val move = new ChangeValue(space.nextMoveId, i, One)
            val after = space.consult(move)
            assertEq(after.value(y), Two)
            space.commit(move)
            assertEq(now.value(y), Two)
        }
        space.initialize
        assertEq(now.value(y), Two)
    }

}
