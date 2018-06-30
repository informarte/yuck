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
        val s = space.createVariable("s", d)
        val t = space.createVariable("t", d)
        val u = space.createVariable("u", d)
        val v = space.createVariable("v", d)
        val costs = space.createVariable("costs", CompleteBooleanDomain)
        val c =
            new LexLessEq(
                space.constraintIdFactory.nextId, null,
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
            val move = new ChangeValue(space.moveIdFactory.nextId, t, Five)
            val after = space.consult(move)
            assertEq(now.value(t), One)
            assertEq(now.value(costs), True)
            assertEq(after.value(t), Five)
            assertEq(after.value(costs), False4)
            space.commit(move)
            assertEq(now.value(t), Five)
            assertEq(now.value(costs), False4)
        }
        if (true) {
            // s = 0
            val move = new ChangeValue(space.moveIdFactory.nextId, s, Zero)
            val after = space.consult(move)
            assertEq(now.value(s), One)
            assertEq(now.value(costs), False4)
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
