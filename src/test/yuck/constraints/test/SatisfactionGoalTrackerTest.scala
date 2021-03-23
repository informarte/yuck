package yuck.constraints.test

import org.junit._

import scala.language.existentials

import yuck.constraints._
import yuck.core._
import yuck.test.util.UnitTest

/**
 * @author Michael Marte
 *
 */
@FixMethodOrder(runners.MethodSorters.NAME_ASCENDING)
class SatisfactionGoalTrackerTest extends UnitTest {

    @Test
    def testTracking: Unit = {
        val space = new Space(logger, sigint)
        val Vector(c1, c2, c3) =
            (0 to 2).map(i => new BooleanVariable(space.nextVariableId, "c%d".format(i), BooleanChannelDomain))
        val m = Map(c1 -> Vector(0, 2), c2 -> Vector(2), c3 -> Vector(0, 1, 2))
        val d = new ArrayBackedDistribution(3)
        space
            .post(new SatisfactionGoalTracker(space.nextConstraintId, None, m, d))
            .setValue(c1, True)
            .setValue(c2, True)
            .setValue(c3, True)
            .initialize()
        assertEq(d.frequency(0), 0)
        assertEq(d.frequency(1), 0)
        assertEq(d.frequency(2), 0)
        if (true) {
            val effects = List(new ImmutableMoveEffect(c1, False))
            val move = new ChangeValues(space.nextMoveId, effects)
            space.consult(move)
            assertEq(d.frequency(0), 0)
            assertEq(d.frequency(1), 0)
            assertEq(d.frequency(2), 0)
            space.commit(move)
            assertEq(d.frequency(0), 1)
            assertEq(d.frequency(1), 0)
            assertEq(d.frequency(2), 1)
        }
        if (true) {
            val effects = List(new ImmutableMoveEffect(c1, True), new ImmutableMoveEffect(c2, False))
            val move = new ChangeValues(space.nextMoveId, effects)
            space.consult(move)
            assertEq(d.frequency(0), 1)
            assertEq(d.frequency(1), 0)
            assertEq(d.frequency(2), 1)
            space.commit(move)
            assertEq(d.frequency(0), 0)
            assertEq(d.frequency(1), 0)
            assertEq(d.frequency(2), 1)
        }
        if (true) {
            val effects = List(new ImmutableMoveEffect(c2, True), new ImmutableMoveEffect(c3, False))
            val move = new ChangeValues(space.nextMoveId, effects)
            space.consult(move)
            assertEq(d.frequency(0), 0)
            assertEq(d.frequency(1), 0)
            assertEq(d.frequency(2), 1)
            space.commit(move)
            assertEq(d.frequency(0), 1)
            assertEq(d.frequency(1), 1)
            assertEq(d.frequency(2), 1)
        }
        if (true) {
            val effects = List(new ImmutableMoveEffect(c1, False), new ImmutableMoveEffect(c2, False))
            val move = new ChangeValues(space.nextMoveId, effects)
            space.consult(move)
            assertEq(d.frequency(0), 1)
            assertEq(d.frequency(1), 1)
            assertEq(d.frequency(2), 1)
            space.commit(move)
            assertEq(d.frequency(0), 2)
            assertEq(d.frequency(1), 1)
            assertEq(d.frequency(2), 3)
        }
    }

}
