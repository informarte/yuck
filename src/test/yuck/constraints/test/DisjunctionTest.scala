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
        val d = CompleteBooleanDomain
        val s = space.createVariable("s", d)
        val t = space.createVariable("t", d)
        val u = space.createVariable("u", d)
        val costs = space.createVariable("costs", CompleteBooleanDomain)
        val c = new Disjunction(space.constraintIdFactory.nextId, null, List(s, t, u), costs)
        space
            .post(c)
            .setValue(s, BooleanValue.get(1))
            .setValue(t, BooleanValue.get(2))
            .setValue(u, BooleanValue.get(3))
            .initialize
        assertEq(space.searchVariables, Set(s, t, u))
        val now = space.searchState
        assertEq(now.value(costs), BooleanValue.get(2))
        if (true) {
            val move = new ChangeValue(space.moveIdFactory.nextId, u, BooleanValue.get(2))
            val after = space.consult(move)
            assertEq(now.value(u), False3)
            assertEq(now.value(costs), False2)
            assertEq(after.value(u), False2)
            assertEq(after.value(costs), False)
            space.commit(move)
            assertEq(now.value(u), False2)
            assertEq(now.value(costs), False)
        }
        if (true) {
            val move = new ChangeValue(space.moveIdFactory.nextId, s, True)
            val after = space.consult(move)
            assertEq(now.value(s), False)
            assertEq(now.value(costs), False)
            assertEq(after.value(s).violation, 0)
            assertEq(after.value(costs).violation, 0)
            space.commit(move)
            assertEq(now.value(s).violation, 0)
            assertEq(now.value(costs).violation, 0)
        }
        if (true) {
            val move = new ChangeValue(space.moveIdFactory.nextId, s, BooleanValue.get(3))
            val after = space.consult(move)
            assertEq(now.value(s).violation, 0)
            assertEq(now.value(costs).violation, 0)
            assertEq(after.value(s), False3)
            assertEq(after.value(costs), False2)
            space.commit(move)
            assertEq(now.value(s), False3)
            assertEq(now.value(costs), False2)
        }
        space.initialize
        assertEq(now.value(costs), False2)
    }

}
