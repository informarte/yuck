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
final class RegularTest extends UnitTest {

    @Test
    def testRegular: Unit = {
        val space = new Space(logger)
        val d = new IntegerRange(One, Two)
        val xs = for (i <- 1 to 10) yield new IntegerVariable(space.nextVariableId, "x[%d]".format(i), d)
        val Q = 6
        val S = 2
        val delta = immutable.IndexedSeq(1, 2, 3, 0, 3, 4, 0, 5, 0, 6, 6, 0).grouped(2).toIndexedSeq
        val q0 = 1
        val F = new IntegerRange(Six, Six)
        val costs = new BooleanVariable(space.nextVariableId, "costs", CompleteBooleanDomain)
        val c = new Regular(space.nextConstraintId, null, xs, Q, S, delta, q0, F, costs)
        space
            .post(c)
            // input:  1, 1, 1, 2, 1, 2, 2, 2, 1, 1
            // states: 1, 1, 1, 2, 3, 4, 5, 6, 6, 6
            .setValue(xs(0), One).setValue(xs(1), One).setValue(xs(2), One).setValue(xs(3), Two)
            .setValue(xs(4), One).setValue(xs(5), Two).setValue(xs(6), Two).setValue(xs(7), Two)
            .setValue(xs(8), One).setValue(xs(9), One)
            .initialize
        assertEq(space.searchVariables, xs.toSet)
        val now = space.searchState
        assertEq(now.value(costs).violation, 0)
        if (true) {
            // input:  1, 1, 1, 2, 2, 2, 2, 2, 1, 1
            // states: 1, 1, 1, 2, 0, 0, 0, 0, 0, 0
            val move = new ChangeValue(space.nextMoveId, xs(4), Two)
            val after = space.consult(move)
            assertEq(after.value(costs), False6)
            space.commit(move)
            assertEq(now.value(costs), False6)
        }
        if (true) {
            // input:  1, 1, 1, 2, 1, 2, 2, 2, 1, 2
            // states: 1, 1, 1, 2, 3, 4, 5, 6, 6, 0
            val move =
            new ChangeValues(
                space.nextMoveId,
                List(new ImmutableEffect(xs(4), One), new ImmutableEffect(xs(9), Two)))
            val after = space.consult(move)
            assertEq(after.value(costs), False)
            space.commit(move)
            assertEq(now.value(costs), False)
        }
        if (true) {
            // input:  1, 1, 1, 2, 1, 2, 2, 2, 1, 1
            // states: 1, 1, 1, 2, 3, 4, 5, 6, 6, 6
            val move = new ChangeValue(space.nextMoveId, xs(9), One)
            val after = space.consult(move)
            assertEq(after.value(costs), True)
            space.commit(move)
            assertEq(now.value(costs), True)
        }
        space.initialize
        assertEq(now.value(costs), True)
    }

}
