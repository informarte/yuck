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
    def testMaximum {
        val space = new Space(logger)
        val d = new IntegerRange(Zero, Nine)
        val s = space.createVariable("s", d)
        val t = space.createVariable("t", d)
        val u = space.createVariable("u", d)
        val max = space.createVariable("costs", CompleteIntegerRange)
        val c = new Maximum(space.constraintIdFactory.nextId, null, List(s, t, u), max)
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
            val move = new ChangeValue(space.moveIdFactory.nextId, u, Two)
            val after = space.consult(move)
            assertEq(after.value(u), Two)
            assertEq(after.value(max), Two)
            space.commit(move)
            assertEq(now.value(u), Two)
            assertEq(now.value(max), Two)
        }
        if (true) {
            val move = new ChangeValue(space.moveIdFactory.nextId, s, Three)
            val after = space.consult(move)
            assertEq(after.value(s), Three)
            assertEq(after.value(max), Three)
            space.commit(move)
            assertEq(now.value(s), Three)
            assertEq(now.value(max), Three)
        }
        space.initialize
        assertEq(now.value(max), Three)
    }

}
