package yuck.constraints.test

import org.junit._

import yuck.constraints._
import yuck.core._
import yuck.util.testing.UnitTest

/**
 * @author Michael Marte
 *
 */
@Test
@FixMethodOrder(runners.MethodSorters.NAME_ASCENDING)
final class NumberOfDistinctValuesTest extends UnitTest {

    @Test
    def testNumberOfDistinctValues {
        val space = new Space(logger)
        val d = new IntegerRange(One, Two)
        val xs = for (i <- 1 to 3) yield space.createVariable("x[%d]".format(i), d)
        val m = new IntegerVariable(space.nextVariableId, "m", NonNegativeIntegerRange)
        val c = new NumberOfDistinctValues(space.nextConstraintId, null, xs, m)
        space
            .post(c)
            .setValue(xs(0), One).setValue(xs(1), One).setValue(xs(2), One)
            .initialize
        val now = space.searchState
        assertEq(now.value(m), One)
        if (true) {
            val move = new ChangeValue(space.nextMoveId, xs(0), Two)
            val after = space.consult(move)
            assertEq(after.value(xs(0)), Two)
            assertEq(after.value(m), Two)
            space.commit(move)
            assertEq(now.value(xs(0)), Two)
            assertEq(now.value(m), Two)
        }
        if (true) {
            val move = new ChangeValue(space.nextMoveId, xs(1), Two)
            val after = space.consult(move)
            assertEq(after.value(xs(1)), Two)
            assertEq(after.value(m), Two)
            space.commit(move)
            assertEq(now.value(xs(1)), Two)
            assertEq(now.value(m), Two)
        }
        if (true) {
            val move = new ChangeValue(space.nextMoveId, xs(2), Two)
            val after = space.consult(move)
            assertEq(after.value(xs(2)), Two)
            assertEq(after.value(m), One)
            space.commit(move)
            assertEq(now.value(xs(2)), Two)
            assertEq(now.value(m), One)
        }
        space.initialize
        assertEq(now.value(m), One)
    }

}
