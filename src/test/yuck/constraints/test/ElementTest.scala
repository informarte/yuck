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
    def testElement {
        val space = new Space(logger)
        val d = new IntegerDomain(Zero, Nine)
        val s = space.createVariable("s", d)
        val t = space.createVariable("t", d)
        val u = space.createVariable("u", d)
        val xs = IndexedSeq(s, t, u)
        val i = space.createVariable("i", d)
        val y = space.createVariable("costs", NonNegativeIntegerDomain)
        val c = new Element(space.constraintIdFactory.nextId, null, xs, i, y, 0)
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
            val move = new ChangeValue(space.moveIdFactory.nextId, s, Zero)
            val after = space.consult(move)
            assertEq(now.value(s), One)
            assertEq(now.value(y), One)
            assertEq(after.value(s), Zero)
            assertEq(after.value(y), Zero)
            space.commit(move)
            assertEq(now.value(s), Zero)
            assertEq(now.value(y), Zero)
        }
        if (true) {
            val move = new ChangeValue(space.moveIdFactory.nextId, i, One)
            val after = space.consult(move)
            assertEq(now.value(i), Zero)
            assertEq(now.value(y), Zero)
            assertEq(after.value(i), One)
            assertEq(after.value(y), Two)
            space.commit(move)
            assertEq(now.value(i), One)
            assertEq(now.value(y), Two)
        }
        space.initialize
        assertEq(now.value(y), Two)
    }

}
