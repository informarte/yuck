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
        val s = space.createVariable("s", d)
        val t = space.createVariable("t", d)
        val u = space.createVariable("u", d)
        val min = space.createVariable("costs", CompleteIntegerRange)
        val c = new Minimum(space.constraintIdFactory.nextId, null, List(s, t, u), min)
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
            val move = new ChangeValue(space.moveIdFactory.nextId, s, Two)
            val after = space.consult(move)
            assertEq(after.value(s), Two)
            assertEq(after.value(min), Two)
            space.commit(move)
            assertEq(now.value(s), Two)
            assertEq(now.value(min), Two)
        }
        if (true) {
            val move = new ChangeValue(space.moveIdFactory.nextId, u, One)
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
