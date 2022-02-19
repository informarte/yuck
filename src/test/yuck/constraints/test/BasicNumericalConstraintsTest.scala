package yuck.constraints.test

import org.junit.*

import scala.collection.*

import yuck.constraints.*
import yuck.core.*
import yuck.test.util.UnitTest

/**
 * @author Michael Marte
 *
 */
@FixMethodOrder(runners.MethodSorters.NAME_ASCENDING)
final class BasicNumericalConstraintsTest extends UnitTest {

    @Test
    def testPlus(): Unit = {
        val space = new Space(logger, sigint)
        val d = IntegerRange(Zero, Nine)
        val s = new IntegerVariable(space.nextVariableId(), "s", d)
        val t = new IntegerVariable(space.nextVariableId(), "t", d)
        val u = new IntegerVariable(space.nextVariableId(), "u", d)
        val v = new IntegerVariable(space.nextVariableId(), "v", d)
        val w = new IntegerVariable(space.nextVariableId(), "w", d)
        // u = s + t
        space
            .post(new Plus(space.nextConstraintId(), null, s, t, u))
            .setValue(s, One)
            .setValue(t, Two)
            .post(new Plus(space.nextConstraintId(), null, u, v, w))
            .setValue(v, One)
            .initialize()
        assertEq(Set(s, t, v), space.searchVariables)
        val now = space.searchState
        assertEq(now.value(u), Three)
        assertEq(now.value(w), Four)
        val move = new ChangeValue(space.nextMoveId(), s, Two)
        val after = space.consult(move)
        assertEq(after.value(u), Four)
        assertEq(after.value(w), Five)
        space.commit(move)
        assertEq(now.value(u), Four)
        assertEq(now.value(w), Five)
        space.initialize()
        assertEq(now.value(w), Five)
    }

}
