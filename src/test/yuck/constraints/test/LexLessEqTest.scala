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
final class LexLessEqTest extends UnitTest {

    @Test
    def testLexLessEq {
        val space = new Space(logger)
        val d = new IntegerRange(Zero, Nine)
        val s = new IntegerVariable(space.nextVariableId, "s", d)
        val t = new IntegerVariable(space.nextVariableId, "t", d)
        val u = new IntegerVariable(space.nextVariableId, "u", d)
        val v = new IntegerVariable(space.nextVariableId, "v", d)
        val costs = new BooleanVariable(space.nextVariableId, "costs", CompleteBooleanDomain)
        val c =
            new LexLessEq(
                space.nextConstraintId, null,
                immutable.IndexedSeq(s, t), immutable.IndexedSeq(u, v), costs)
        space
            .post(c)
            .setValue(s, One)
            .setValue(t, One)
            .setValue(u, One)
            .setValue(v, One)
            .initialize
        assertEq(space.searchVariables, Set(s, t, u, v))
        val now = space.searchState
        assertEq(now.value(costs), True)
        if (true) {
            // t = 5
            val move = new ChangeValue(space.nextMoveId, t, Five)
            val after = space.consult(move)
            assertEq(after.value(t), Five)
            assertEq(after.value(costs), False4)
            space.commit(move)
            assertEq(now.value(t), Five)
            assertEq(now.value(costs), False4)
        }
        if (true) {
            // s = 0
            val move = new ChangeValue(space.nextMoveId, s, Zero)
            val after = space.consult(move)
            assertEq(after.value(s), Zero)
            assertEq(after.value(costs), True)
            space.commit(move)
            assertEq(now.value(s), Zero)
            assertEq(now.value(costs), True)
        }
        space.initialize
        assertEq(now.value(costs), True)
    }

}
