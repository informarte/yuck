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
        val s = new IntegerVariable(space.variableIdFactory.nextId, "s", d)
        val t = new IntegerVariable(space.variableIdFactory.nextId, "t", d)
        val u = new IntegerVariable(space.variableIdFactory.nextId, "u", d)
        val v = new IntegerVariable(space.variableIdFactory.nextId, "v", d)
        val w = new IntegerVariable(space.variableIdFactory.nextId, "w", d)
        // u = s + t
        space
            .post(new Plus(space.constraintIdFactory.nextId, null, s, t, u))
            .setValue(s, One)
            .setValue(t, Two)
            .post(new Plus(space.constraintIdFactory.nextId, null, u, v, w))
            .setValue(v, One)
            .initialize
        assertEq(Set(s, t, v), space.searchVariables)
        val now = space.searchState
        assertEq(now.value(u), Three)
        assertEq(now.value(w), Four)
        val move = new ChangeValue(space.moveIdFactory.nextId, s, Two)
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
