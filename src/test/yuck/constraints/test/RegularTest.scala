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
    def testRegular {
        val space = new Space(logger)
        val d = new IntegerRange(One, Two)
        val xs = for (i <- 1 to 10) yield space.createVariable("x[%d]".format(i), d)
        val Q = 6
        val S = 2
        val delta = immutable.IndexedSeq(1, 2, 3, 0, 3, 4, 0, 5, 0, 6, 6, 0).grouped(2).toIndexedSeq
        val q0 = 1
        val F = new IntegerRange(Six, Six)
        val costs = space.createVariable("costs", NonNegativeIntegerRange)
        val c = new Regular(space.constraintIdFactory.nextId, null, xs, Q, S, delta, q0, F, costs)
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
        assertEq(now.value(costs), Zero)
        if (true) {
            // input:  1, 1, 1, 2, 2, 2, 2, 2, 1, 1
            // states: 1, 1, 1, 2, 0, 0, 0, 0, 0, 0
            val move = new ChangeValue(space.moveIdFactory.nextId, xs(4), Two)
            val after = space.consult(move)
            assertEq(now.value(xs(4)), One)
            assertEq(now.value(costs), Zero)
            assertEq(after.value(xs(4)), Two)
            assertEq(after.value(costs), Six)
            space.commit(move)
            assertEq(now.value(xs(4)), Two)
            assertEq(now.value(costs), Six)
        }
        if (true) {
            // input:  1, 1, 1, 2, 1, 2, 2, 2, 1, 2
            // states: 1, 1, 1, 2, 3, 4, 5, 6, 6, 0
            val move =
            new ChangeValues(
                space.moveIdFactory.nextId,
                List(new ImmutableEffect(xs(4), One), new ImmutableEffect(xs(9), Two)))
            val after = space.consult(move)
            assertEq(now.value(xs(4)), Two)
            assertEq(now.value(xs(9)), One)
            assertEq(now.value(costs), Six)
            assertEq(after.value(xs(4)), One)
            assertEq(after.value(xs(9)), Two)
            assertEq(after.value(costs), One)
            space.commit(move)
            assertEq(now.value(xs(4)), One)
            assertEq(now.value(xs(9)), Two)
            assertEq(now.value(costs), One)
        }
        if (true) {
            // input:  1, 1, 1, 2, 1, 2, 2, 2, 1, 1
            // states: 1, 1, 1, 2, 3, 4, 5, 6, 6, 6
            val move = new ChangeValue(space.moveIdFactory.nextId, xs(9), One)
            val after = space.consult(move)
            assertEq(now.value(xs(9)), Two)
            assertEq(now.value(costs), One)
            assertEq(after.value(xs(9)), One)
            assertEq(after.value(costs), Zero)
            space.commit(move)
            assertEq(now.value(xs(9)), One)
            assertEq(now.value(costs), Zero)
        }
        space.initialize
        assertEq(now.value(costs), Zero)
    }

}
