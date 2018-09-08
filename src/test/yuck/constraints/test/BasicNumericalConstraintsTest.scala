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
final class BasicNumericalConstraintsTest extends UnitTest {

    @Test
    def testPlus {
        val space = new Space(logger)
        val d = new IntegerRange(Zero, Nine)
        val s = space.createVariable("s", d)
        val t = space.createVariable("t", d)
        val u = space.createVariable("u", d)
        val v = space.createVariable("v", d)
        val w = space.createVariable("w", d)
        // u = s + t
        space
            .post(new Plus(space.constraintIdFactory.nextId, null, s, t, u))
            .setValue(s, new IntegerValue(1))
            .setValue(t, new IntegerValue(2))
            .post(new Plus(space.constraintIdFactory.nextId, null, u, v, w))
            .setValue(v, new IntegerValue(1))
            .initialize
        assertEq(Set(s, t, v), space.searchVariables)
        val now = space.searchState
        assertEq(now.value(u), Three)
        assertEq(now.value(w), Four)
        val move = new ChangeValue(space.moveIdFactory.nextId, s, new IntegerValue(2))
        val after = space.consult(move)
        assertEq(after.value(u), Four)
        assertEq(after.value(w), Five)
        space.commit(move)
        assertEq(now.value(u), Four)
        assertEq(now.value(w), Five)
        space.initialize
        assertEq(now.value(w), Five)
    }

}
