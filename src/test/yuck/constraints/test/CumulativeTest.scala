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
final class CumulativeTest extends UnitTest {

    @Test
    def testCumulative {
        val space = new Space(logger)
        val d = new IntegerRange(Zero, Nine)
        val s0 = space.createVariable("s0", d)
        val d0 = space.createVariable("d0", d)
        val c0 = space.createVariable("c0", d)
        val s1 = space.createVariable("s1", d)
        val d1 = space.createVariable("d1", d)
        val c1 = space.createVariable("c1", d)
        val t0 = new CumulativeTask(s0, d0, c0)
        val t1 = new CumulativeTask(s1, d1, c1)
        val ub = space.createVariable("ub", d)
        val costs = space.createVariable("costs", NonNegativeIntegerRange)
        val c = new Cumulative(space.constraintIdFactory.nextId, null, List(t0, t1), ub, costs)
        // initial conflict: two tasks with same start time and duration
        space
            .post(c)
            .setValue(s0, Zero)
            .setValue(d0, Three)
            .setValue(c0, Two)
            .setValue(s1, Zero)
            .setValue(d1, Three)
            .setValue(c1, Two)
            .setValue(ub, Three)
            .initialize
        assertEq(space.searchVariables, Set(s0, d0, c0, s1, d1, c1, ub))
        val now = space.searchState
        assertEq(now.value(costs), Three)
        if (true) {
            // move second task to resolve conflict
            val move = new ChangeValue(space.moveIdFactory.nextId, s1, Three)
            val after = space.consult(move)
            assertEq(now.value(s1), Zero)
            assertEq(now.value(costs), Three)
            assertEq(after.value(s1), Three)
            assertEq(after.value(costs), Zero)
            space.commit(move)
            assertEq(now.value(s1), Three)
            assertEq(now.value(costs), Zero)
        }
        if (true) {
            // make first task longer to create overlap
            val move = new ChangeValue(space.moveIdFactory.nextId, d0, Four)
            val after = space.consult(move)
            assertEq(now.value(d0), Three)
            assertEq(now.value(costs), Zero)
            assertEq(after.value(d0), Four)
            assertEq(after.value(costs), One)
            space.commit(move)
            assertEq(now.value(d0), Four)
            assertEq(now.value(costs), One)
        }
        if (true) {
            // resolve conflict by increasing resource supply
            val move = new ChangeValue(space.moveIdFactory.nextId, ub, Four)
            val after = space.consult(move)
            assertEq(now.value(ub), Three)
            assertEq(now.value(costs), One)
            assertEq(after.value(ub), Four)
            assertEq(after.value(costs), Zero)
            space.commit(move)
            assertEq(now.value(ub), Four)
            assertEq(now.value(costs), Zero)
        }
        if (true) {
            // change duration and resource consumption in one move
            val move =
                new ChangeValues(
                    space.moveIdFactory.nextId,
                    List(new ImmutableEffect(d0, Three), new ImmutableEffect(c0, Five)))
            val after = space.consult(move)
            assertEq(now.value(d0), Four)
            assertEq(now.value(c0), Two)
            assertEq(now.value(costs), Zero)
            assertEq(after.value(d0), Three)
            assertEq(after.value(c0), Five)
            assertEq(after.value(costs), Three)
            space.commit(move)
            assertEq(now.value(d0), Three)
            assertEq(now.value(c0), Five)
            assertEq(now.value(costs), Three)
        }
        space.initialize
        assertEq(now.value(costs), Three)
    }

}
