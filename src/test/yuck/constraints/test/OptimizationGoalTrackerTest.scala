package yuck.constraints.test

import org.junit.*

import scala.collection.*

import yuck.constraints.*
import yuck.core.{given, *}
import yuck.test.util.UnitTest

/**
 * @author Michael Marte
 *
 */
@FixMethodOrder(runners.MethodSorters.NAME_ASCENDING)
class OptimizationGoalTrackerTest extends UnitTest {

    @Test
    def testMinimizationMode(): Unit = {
        val d = new ArrayBackedDistribution(2)
        val space = new Space(logger, sigint)
        val dx = IntegerRange(IntegerValue(-10), IntegerValue(50))
        val dy = IntegerRange(IntegerValue(0), IntegerValue(100))
        val x = new IntegerVariable(space.nextVariableId(), "x", dx)
        val y = new IntegerVariable(space.nextVariableId(), "y", dy)
        val axs = Vector(new AX(Three, x), new AX(IntegerValue(-2), y))
        val c = new OptimizationGoalTracker(space.nextConstraintId(), null, OptimizationMode.Min, axs, d)
        space
            .post(c)
            .setValue(x, One)
            .setValue(y, Two)
            .initialize()
        assertEq(d.frequency(0), 33)
        assertEq(d.frequency(1), 196)
        if (true) {
            // change values a bit
            val effects = List(new ImmutableMoveEffect(x, Five), new ImmutableMoveEffect(y, Ten))
            val move = new ChangeValues(space.nextMoveId(), effects)
            space.consult(move)
            assertEq(d.frequency(0), 33)
            assertEq(d.frequency(1), 196)
            space.commit(move)
            assertEq(d.frequency(0), 45)
            assertEq(d.frequency(1), 180)
        }
        if (true) {
            // let scalar combination take optimum value
            val effects = List(new ImmutableMoveEffect(x, dx.lb), new ImmutableMoveEffect(y, dy.ub))
            val move = new ChangeValues(space.nextMoveId(), effects)
            space.consult(move)
            assertEq(d.frequency(0), 45)
            assertEq(d.frequency(1), 180)
            space.commit(move)
            assertEq(d.frequency(0), 0)
            assertEq(d.frequency(1), 0)
        }
        if (true) {
            // let variables take values outside their domains
            val effects = List(new ImmutableMoveEffect(x, dx.lb - One), new ImmutableMoveEffect(y, dy.ub + One))
            val move = new ChangeValues(space.nextMoveId(), effects)
            space.consult(move)
            assertEq(d.frequency(0), 0)
            assertEq(d.frequency(1), 0)
            space.commit(move)
            assertEq(d.frequency(0), 3)
            assertEq(d.frequency(1), 2)
        }
    }

    @Test
    def testMaximizationMode(): Unit = {
        val d = new ArrayBackedDistribution(2)
        val space = new Space(logger, sigint)
        val dx = IntegerRange(IntegerValue(0), IntegerValue(100))
        val dy = IntegerRange(IntegerValue(-10), IntegerValue(50))
        val x = new IntegerVariable(space.nextVariableId(), "x", dx)
        val y = new IntegerVariable(space.nextVariableId(), "y", dy)
        val axs = Vector(new AX(Three, x), new AX(IntegerValue(-2), y))
        val c = new OptimizationGoalTracker(space.nextConstraintId(), null, OptimizationMode.Max, axs, d)
        space
            .post(c)
            .setValue(x, One)
            .setValue(y, Two)
            .initialize()
        assertEq(d.frequency(0), 297)
        assertEq(d.frequency(1), 24)
        if (true) {
            // change values a bit
            val effects = List(new ImmutableMoveEffect(x, Five), new ImmutableMoveEffect(y, Ten))
            val move = new ChangeValues(space.nextMoveId(), effects)
            space.consult(move)
            assertEq(d.frequency(0), 297)
            assertEq(d.frequency(1), 24)
            space.commit(move)
            assertEq(d.frequency(0), 285)
            assertEq(d.frequency(1), 40)
        }
        if (true) {
            // let scalar combination take optimum value
            val effects = List(new ImmutableMoveEffect(x, dx.ub), new ImmutableMoveEffect(y, dy.lb))
            val move = new ChangeValues(space.nextMoveId(), effects)
            space.consult(move)
            assertEq(d.frequency(0), 285)
            assertEq(d.frequency(1), 40)
            space.commit(move)
            assertEq(d.frequency(0), 0)
            assertEq(d.frequency(1), 0)
        }
        if (true) {
            // let variables take values outside their domains
            val effects = List(new ImmutableMoveEffect(x, dx.ub + One), new ImmutableMoveEffect(y, dy.lb - One))
            val move = new ChangeValues(space.nextMoveId(), effects)
            space.consult(move)
            assertEq(d.frequency(0), 0)
            assertEq(d.frequency(1), 0)
            space.commit(move)
            assertEq(d.frequency(0), 3)
            assertEq(d.frequency(1), 2)
        }
    }

}
