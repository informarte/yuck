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
    def testCountVar {
        val space = new Space(logger)
        val d = new IntegerRange(Zero, Nine)
        val s = space.createVariable("s", d)
        val t = space.createVariable("t", d)
        val u = space.createVariable("u", d)
        val what = space.createVariable("what", d)
        val n = space.createVariable("n", NonNegativeIntegerRange)
        val c = new CountVar(space.constraintIdFactory.nextId, null, List(s, t, u), what, n)
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
            val move = new ChangeValue(space.moveIdFactory.nextId, s, Two)
            val after = space.consult(move)
            assertEq(after.value(s), Two)
            assertEq(after.value(n), Two)
            space.commit(move)
            assertEq(now.value(s), Two)
            assertEq(now.value(n), Two)
        }
        if (true) {
            val move = new ChangeValue(space.moveIdFactory.nextId, what, Two)
            val after = space.consult(move)
            assertEq(after.value(what), Two)
            assertEq(after.value(n), One)
            space.commit(move)
            assertEq(now.value(what), Two)
            assertEq(now.value(n), One)
        }
        space.initialize
        assertEq(now.value(n), One)
    }

}
