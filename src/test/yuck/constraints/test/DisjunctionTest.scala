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
final class DisjunctionTest extends UnitTest {

    @Test
    def testDisjunction {
        val space = new Space(logger)
        val d = new IntegerRange(Zero, Nine)
        val s = space.createVariable("s", d)
        val t = space.createVariable("t", d)
        val u = space.createVariable("u", d)
        val costs = space.createVariable("costs", CompleteIntegerRange)
        val c = new Disjunction(space.constraintIdFactory.nextId, null, List(s, t, u), costs)
        space
            .post(c)
            .setValue(s, One)
            .setValue(t, Two)
            .setValue(u, Three)
            .initialize
        assertEq(space.searchVariables, Set(s, t, u))
        val now = space.searchState
        assertEq(now.value(costs), Two)
        if (true) {
            val move = new ChangeValue(space.moveIdFactory.nextId, u, Two)
            val after = space.consult(move)
            assertEq(now.value(u), Three)
            assertEq(now.value(costs), Two)
            assertEq(after.value(u), Two)
            assertEq(after.value(costs), One)
            space.commit(move)
            assertEq(now.value(u), Two)
            assertEq(now.value(costs), One)
        }
        if (true) {
            val move = new ChangeValue(space.moveIdFactory.nextId, s, Zero)
            val after = space.consult(move)
            assertEq(now.value(s), One)
            assertEq(now.value(costs), One)
            assertEq(after.value(s), Zero)
            assertEq(after.value(costs), Zero)
            space.commit(move)
            assertEq(now.value(s), Zero)
            assertEq(now.value(costs), Zero)
        }
        if (true) {
            val move = new ChangeValue(space.moveIdFactory.nextId, s, Three)
            val after = space.consult(move)
            assertEq(now.value(s), Zero)
            assertEq(now.value(costs), Zero)
            assertEq(after.value(s), Three)
            assertEq(after.value(costs), Two)
            space.commit(move)
            assertEq(now.value(s), Three)
            assertEq(now.value(costs), Two)
        }
        space.initialize
        assertEq(now.value(costs), Two)
    }

}
