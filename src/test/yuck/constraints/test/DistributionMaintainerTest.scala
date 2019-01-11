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
class DistributionMaintainerTest extends UnitTest {

    @Test
    def testMinimizationMode {
        val d = new ArrayBackedDistribution(2)
        val space = new Space(logger)
        val dx = new IntegerRange(IntegerValue.get(-10), IntegerValue.get(50))
        val dy = new IntegerRange(IntegerValue.get(0), IntegerValue.get(100))
        val x = new IntegerVariable(space.nextVariableId, "x", dx)
        val y = new IntegerVariable(space.nextVariableId, "y", dy)
        val axs = immutable.IndexedSeq(new AX(Three, x), new AX(IntegerValue.get(-2), y))
        val c = new DistributionMaintainer(space.nextConstraintId, null, OptimizationMode.Min, axs, d)
        space
            .post(c)
            .setValue(x, One)
            .setValue(y, Two)
            .initialize
        assertEq(d.frequency(0), 33)
        assertEq(d.frequency(1), 196)
        if (true) {
            // change values a bit
            val effects = List(new ImmutableEffect(x, Five), new ImmutableEffect(y, Ten))
            val move = new ChangeValues(space.nextMoveId, effects)
            space.consult(move)
            assertEq(d.frequency(0), 33)
            assertEq(d.frequency(1), 196)
            space.commit(move)
            assertEq(d.frequency(0), 45)
            assertEq(d.frequency(1), 180)
        }
        if (true) {
            // let scalar combination take optimum value
            val effects = List(new ImmutableEffect(x, dx.lb), new ImmutableEffect(y, dy.ub))
            val move = new ChangeValues(space.nextMoveId, effects)
            space.consult(move)
            assertEq(d.frequency(0), 45)
            assertEq(d.frequency(1), 180)
            space.commit(move)
            assertEq(d.frequency(0), 0)
            assertEq(d.frequency(1), 0)
        }
        if (true) {
            // let variables take values outside their domains
            val effects = List(new ImmutableEffect(x, dx.lb - One), new ImmutableEffect(y, dy.ub + One))
            val move = new ChangeValues(space.nextMoveId, effects)
            space.consult(move)
            assertEq(d.frequency(0), 0)
            assertEq(d.frequency(1), 0)
            space.commit(move)
            assertEq(d.frequency(0), 3)
            assertEq(d.frequency(1), 2)
        }
    }

    @Test
    def testMaximizationMode {
        val d = new ArrayBackedDistribution(2)
        val space = new Space(logger)
        val dx = new IntegerRange(IntegerValue.get(0), IntegerValue.get(100))
        val dy = new IntegerRange(IntegerValue.get(-10), IntegerValue.get(50))
        val x = new IntegerVariable(space.nextVariableId, "x", dx)
        val y = new IntegerVariable(space.nextVariableId, "y", dy)
        val axs = immutable.IndexedSeq(new AX(Three, x), new AX(IntegerValue.get(-2), y))
        val c = new DistributionMaintainer(space.nextConstraintId, null, OptimizationMode.Max, axs, d)
        space
            .post(c)
            .setValue(x, One)
            .setValue(y, Two)
            .initialize
        assertEq(d.frequency(0), 297)
        assertEq(d.frequency(1), 24)
        if (true) {
            // change values a bit
            val effects = List(new ImmutableEffect(x, Five), new ImmutableEffect(y, Ten))
            val move = new ChangeValues(space.nextMoveId, effects)
            space.consult(move)
            assertEq(d.frequency(0), 297)
            assertEq(d.frequency(1), 24)
            space.commit(move)
            assertEq(d.frequency(0), 285)
            assertEq(d.frequency(1), 40)
        }
        if (true) {
            // let scalar combination take optimum value
            val effects = List(new ImmutableEffect(x, dx.ub), new ImmutableEffect(y, dy.lb))
            val move = new ChangeValues(space.nextMoveId, effects)
            space.consult(move)
            assertEq(d.frequency(0), 285)
            assertEq(d.frequency(1), 40)
            space.commit(move)
            assertEq(d.frequency(0), 0)
            assertEq(d.frequency(1), 0)
        }
        if (true) {
            // let variables take values outside their domains
            val effects = List(new ImmutableEffect(x, dx.ub + One), new ImmutableEffect(y, dy.lb - One))
            val move = new ChangeValues(space.nextMoveId, effects)
            space.consult(move)
            assertEq(d.frequency(0), 0)
            assertEq(d.frequency(1), 0)
            space.commit(move)
            assertEq(d.frequency(0), 3)
            assertEq(d.frequency(1), 2)
        }
    }

}
