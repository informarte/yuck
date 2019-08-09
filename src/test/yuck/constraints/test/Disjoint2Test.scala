package yuck.constraints.test

import org.junit._

import scala.collection._
import scala.language.existentials

import yuck.constraints._
import yuck.core._
import yuck.util.testing.UnitTest

/**
 * @author Michael Marte
 *
 */
@Test
@FixMethodOrder(runners.MethodSorters.NAME_ASCENDING)
final class Disjoint2Test extends UnitTest {

    private def createRect(space: Space, i: Int, d: IntegerDomain): Disjoint2Rect =
        new Disjoint2Rect(
            new IntegerVariable(space.nextVariableId, "x%d".format(i), d),
            new IntegerVariable(space.nextVariableId, "y%d".format(i), d),
            new IntegerVariable(space.nextVariableId, "w%d".format(i), d),
            new IntegerVariable(space.nextVariableId, "h%d".format(i), d))

    @Test
    def testSearchVariables: Unit = {
        val space = new Space(logger)
        val rects @ Vector(r1, r2) = (1 to 2).map(createRect(space, _, new IntegerRange(Zero, Nine)))
        val costs = new BooleanVariable(space.nextVariableId, "costs", CompleteBooleanDomain)
        space.post(new Disjoint2(space.nextConstraintId, null, rects, false, costs))
        assertEq(space.searchVariables, Set(r1.x, r1.y, r1.w, r1.h, r2.x, r2.y, r2.w, r2.h))
    }

    @Test
    def testRectangleMovement: Unit = {
        val space = new Space(logger)
        val rects @ Vector(r1, r2, r3, r4) = (1 to 4).map(createRect(space, _, new IntegerRange(Zero, Nine)))
        val costs = new BooleanVariable(space.nextVariableId, "costs", CompleteBooleanDomain)
        // initial conflict: r1 and r3 are identical, r3 overlaps with r1 and r2
        space
            .post(new Disjoint2(space.nextConstraintId, null, rects, false, costs))
            .setValue(r1.x, Zero).setValue(r1.y, One).setValue(r1.w, Three).setValue(r1.h, Four)
            .setValue(r2.x, Five).setValue(r2.y, Four).setValue(r2.w, Three).setValue(r2.h, Two) // disjoint from r1
            .setValue(r3.x, Zero).setValue(r3.y, One).setValue(r3.w, Three).setValue(r3.h, Four) // identical to r1
            .setValue(r4.x, Two).setValue(r4.y, Three).setValue(r4.w, Five).setValue(r4.h, Four) // overlaps r1, r2, and r3
            .initialize
        val now = space.searchState
        assertEq(now.value(costs), BooleanValue.get(20))
        if (true) {
            // move r1 from (0, 1) to (3, 1)
            val move = new ChangeValue(space.nextMoveId, r1.x, Three)
            val after = space.consult(move)
            assertEq(after.value(costs), BooleanValue.get(13))
            space.commit(move)
            assertEq(now.value(costs), BooleanValue.get(13))
        }
        if (true) {
            // move r3 from (0, 1) to (0, 3)
            val move = new ChangeValue(space.nextMoveId, r3.y, Three)
            val after = space.consult(move)
            assertEq(after.value(costs), BooleanValue.get(15))
            space.commit(move)
            assertEq(now.value(costs), BooleanValue.get(15))
        }
        if (true) {
            // move r1 and r3 back
            val move = new ChangeValues(space.nextMoveId, List(new ImmutableEffect(r1.x, Zero), new ImmutableEffect(r3.y, One)))
            val after = space.consult(move)
            assertEq(after.value(costs), BooleanValue.get(20))
            space.commit(move)
            assertEq(now.value(costs), BooleanValue.get(20))
        }
        space.initialize
        assertEq(now.value(costs), BooleanValue.get(20))
    }

    @Test
    def testRectangleResizing: Unit = {
        val space = new Space(logger)
        val rects = (1 to 2).map(createRect(space, _, new IntegerRange(Zero, Nine)))
        val Vector(r1, r2) = rects
        val costs = new BooleanVariable(space.nextVariableId, "costs", CompleteBooleanDomain)
        // initial conflict: r2 overlaps with r1
        space
            .post(new Disjoint2(space.nextConstraintId, null, rects, false, costs))
            .setValue(r1.x, Zero).setValue(r1.y, One).setValue(r1.w, Four).setValue(r1.h, Four)
            .setValue(r2.x, Two).setValue(r2.y, Three).setValue(r2.w, Four).setValue(r2.h, Four) // overlaps r1
            .initialize
        val now = space.searchState
        assertEq(now.value(costs), False4)
        if (true) {
            // reduce height and width of r2 to 1
            val move = new ChangeValues(space.nextMoveId, List(new ImmutableEffect(r2.w, One), new ImmutableEffect(r2.h, One)))
            val after = space.consult(move)
            assertEq(after.value(costs), False)
            space.commit(move)
            assertEq(now.value(costs), False)
        }
        if (true) {
            // restore width of r2
            val move = new ChangeValue(space.nextMoveId, r2.w, Four)
            val after = space.consult(move)
            assertEq(after.value(costs), False2)
            space.commit(move)
            assertEq(now.value(costs), False2)
        }
        if (true) {
            // restore height of r2
            val move = new ChangeValue(space.nextMoveId, r2.h, Four)
            val after = space.consult(move)
            assertEq(after.value(costs), False4)
            space.commit(move)
            assertEq(now.value(costs), False4)
        }
        space.initialize
        assertEq(now.value(costs), False4)
    }

    @Test
    def testSimultaneousRectangleMovementAndResizing: Unit = {
        val space = new Space(logger)
        val rects = (1 to 2).map(createRect(space, _, new IntegerRange(Zero, Nine)))
        val Vector(r1, r2) = rects
        val costs = new BooleanVariable(space.nextVariableId, "costs", CompleteBooleanDomain)
        // no initial conflict
        space
            .post(new Disjoint2(space.nextConstraintId, null, rects, false, costs))
            .setValue(r1.x, Zero).setValue(r1.y, Zero).setValue(r1.w, Four).setValue(r1.h, Four)
            .setValue(r2.x, Four).setValue(r2.y, Four).setValue(r2.w, Four).setValue(r2.h, Four)
            .initialize
        val now = space.searchState
        assertEq(now.value(costs), True)
        if (true) {
            // move and resize r1 to partially overlap r2
            val move =
                new ChangeValues(
                    space.nextMoveId,
                    List(new ImmutableEffect(r1.x, Two), new ImmutableEffect(r1.y, Three),
                         new ImmutableEffect(r1.w, Three), new ImmutableEffect(r1.h, Two)))
            val after = space.consult(move)
            assertEq(after.value(costs), False)
            space.commit(move)
            assertEq(now.value(costs), False)
        }
        if (true) {
            // move r2 and resize r1
            val move =
                new ChangeValues(
                    space.nextMoveId,
                    List(new ImmutableEffect(r1.w, Four),
                         new ImmutableEffect(r2.x, Three), new ImmutableEffect(r2.y, Two)))
            val after = space.consult(move)
            assertEq(after.value(costs), False6)
            space.commit(move)
            assertEq(now.value(costs), False6)
        }
        space.initialize
        assertEq(now.value(costs), False6)
    }

    @Test
    def testHandlingOfAdjacentRectangles: Unit = {
        val space = new Space(logger)
        val rects = (1 to 3).map(createRect(space, _, new IntegerRange(Zero, Nine)))
        val Vector(r1, r2, r3) = rects
        val costs = new BooleanVariable(space.nextVariableId, "costs", CompleteBooleanDomain)
        space
            .post(new Disjoint2(space.nextConstraintId, null, rects, false, costs))
            .setValue(r1.x, Zero).setValue(r1.y, Zero).setValue(r1.w, One).setValue(r1.h, One)
            .setValue(r2.x, Zero).setValue(r2.y, One).setValue(r2.w, One).setValue(r2.h, One) // adjacent to r1
            .setValue(r3.x, One).setValue(r3.y, Zero).setValue(r3.w, One).setValue(r3.h, Two) // adjacent to r1 and r2
            .initialize
        val now = space.searchState
        assertEq(now.value(costs), True)
    }

    @Test
    def testHandlingOfSharedVariables: Unit = {
        val space = new Space(logger)
        val d = new IntegerRange(Zero, Nine)
        val r1 = createRect(space, 1, d)
        val r2 =
            new Disjoint2Rect(
                r1.x,
                new IntegerVariable(space.nextVariableId, "y2", d),
                r1.w,
                new IntegerVariable(space.nextVariableId, "h2", d))
        val r3 = createRect(space, 3, d)
        val costs = new BooleanVariable(space.nextVariableId, "costs", CompleteBooleanDomain)
        // initial conflict because r1 occurs twice in c
        space
            .post(new Disjoint2(space.nextConstraintId, null, Vector(r1, r2, r3, r1), false, costs))
            .setValue(r1.x, Zero).setValue(r1.y, Zero).setValue(r1.w, Two).setValue(r1.h, Two)
            .setValue(r2.y, Two).setValue(r2.h, Two) // adjacent to r1
            .setValue(r3.x, Two).setValue(r3.y, Zero).setValue(r3.w, Two).setValue(r3.h, Four) // adjacent to r1 and r2
            .initialize
        val now = space.searchState
        assertEq(now.value(costs), False4)
        if (true) {
            // move r1 and r2 to partially overlap r3
            val move = new ChangeValue(space.nextMoveId, r1.x, One)
            val after = space.consult(move)
            assertEq(after.value(costs), False10)
            space.commit(move)
            assertEq(now.value(costs), False10)
        }
        if (true) {
            // make r1 and r2 wider to fully overlap r3
            val move = new ChangeValue(space.nextMoveId, r1.w, Four)
            val after = space.consult(move)
            assertEq(after.value(costs), BooleanValue.get(20))
            space.commit(move)
            assertEq(now.value(costs), BooleanValue.get(20))
        }
        space.initialize
        assertEq(now.value(costs), BooleanValue.get(20))
    }

    @Test
    def testConsultWithoutCommit: Unit = {
        val space = new Space(logger)
        val rects = (1 to 2).map(createRect(space, _, new IntegerRange(Zero, Nine)))
        val Vector(r1, r2) = rects
        val costs = new BooleanVariable(space.nextVariableId, "costs", CompleteBooleanDomain)
        // initial conflict: r2 overlaps with r1
        space
            .post(new Disjoint2(space.nextConstraintId, null, rects, false, costs))
            .setValue(r1.x, Zero).setValue(r1.y, One).setValue(r1.w, Four).setValue(r1.h, Four)
            .setValue(r2.x, Two).setValue(r2.y, Three).setValue(r2.w, Four).setValue(r2.h, Four) // overlaps r1
            .initialize
        val now = space.searchState
        assertEq(now.value(costs), False4)
        if (true) {
            // reduce height and width of r2 to 1
            val move = new ChangeValues(space.nextMoveId, List(new ImmutableEffect(r2.w, One), new ImmutableEffect(r2.h, One)))
            val after = space.consult(move)
            assertEq(after.value(costs), False)
        }
        if (true) {
            // move r1 to fully overlap r2
            val move = new ChangeValues(space.nextMoveId, List(new ImmutableEffect(r1.x, Two), new ImmutableEffect(r1.y, Three)))
            val after = space.consult(move)
            assertEq(after.value(costs), BooleanValue.get(16))
        }
        space.initialize
        assertEq(now.value(costs), False4)
    }

    @Test
    def testNonStrictSemantics: Unit = {
        val space = new Space(logger)
        val rects = (1 to 2).map(createRect(space, _, new IntegerRange(Zero, Nine)))
        val Vector(r1, r2) = rects
        val costs = new BooleanVariable(space.nextVariableId, "costs", CompleteBooleanDomain)
        // initial conflict: r2 overlaps with r1
        space
            .post(new Disjoint2(space.nextConstraintId, null, rects, false, costs))
            .setValue(r1.x, Zero).setValue(r1.y, One).setValue(r1.w, Four).setValue(r1.h, Four)
            .setValue(r2.x, Two).setValue(r2.y, Three).setValue(r2.w, Four).setValue(r2.h, Four) // overlaps r1
            .initialize
        val now = space.searchState
        assertEq(now.value(costs), False4)
        if (true) {
            // reduce width of r2 to 0
            val move = new ChangeValue(space.nextMoveId, r2.w, Zero)
            val after = space.consult(move)
            assertEq(after.value(costs), True)
            space.commit(move)
            assertEq(now.value(costs), True)
        }
        if (true) {
            // restore width of r2
            val move = new ChangeValue(space.nextMoveId, r2.w, Four)
            val after = space.consult(move)
            assertEq(after.value(costs), False4)
            space.commit(move)
            assertEq(now.value(costs), False4)
        }
        space.initialize
        assertEq(now.value(costs), False4)
    }

    @Test
    def testRectangleMovementWithStrictSemantics: Unit = {
        val space = new Space(logger)
        val rects = (1 to 9).map(createRect(space, _, new IntegerRange(Zero, Nine)))
        val Vector(r1, r2, r3, r4, vl1, vl2, hl1, hl2, p1) = rects
        val costs = new BooleanVariable(space.nextVariableId, "costs", CompleteBooleanDomain)
        // setup:
        // - four rectangles plus two vertical lines plus two horizontal lines
        // - vertical lines coincide on the left of the rectangles
        // - horizontal lines coincide below the rectangles (so they do not intersect the vertical lines)
        /*
         ****** ******
         * r3 * * r4 *
         ****** ******
         ****** ******
         * r1 * * r2 *
         ****** ******
         */
        space
            .post(new Disjoint2(space.nextConstraintId, null, rects, true, costs))
            // r1 through r4 are adjacent rectangles
            .setValue(r1.x, Zero).setValue(r1.y, Zero).setValue(r1.w, Two).setValue(r1.h, Two)
            .setValue(r2.x, Two).setValue(r2.y, Zero).setValue(r2.w, Two).setValue(r2.h, Two)
            .setValue(r3.x, Zero).setValue(r3.y, Two).setValue(r3.w, Two).setValue(r3.h, Two)
            .setValue(r4.x, Two).setValue(r4.y, Two).setValue(r4.w, Two).setValue(r4.h, Two)
            // vl1 and vl2 are vertical lines
            .setValue(vl1.x, Zero).setValue(vl1.y, Zero).setValue(vl1.w, Zero).setValue(vl1.h, Four)
            .setValue(vl2.x, Zero).setValue(vl2.y, Zero).setValue(vl2.w, Zero).setValue(vl2.h, Four)
            // hl1 and hl2 are horizontal lines
            .setValue(hl1.x, Zero).setValue(hl1.y, Zero).setValue(hl1.w, Four).setValue(hl1.h, Zero)
            .setValue(hl2.x, Zero).setValue(hl2.y, Zero).setValue(hl2.w, Four).setValue(hl2.h, Zero)
            // p1 is a point
            .setValue(p1.x, Zero).setValue(p1.y, Zero).setValue(p1.w, Zero).setValue(p1.h, Zero)
            .initialize
        val now = space.searchState
        assertEq(now.value(costs), True)
        if (true) {
            // make vl1 intersect r1 and r3
            val move = new ChangeValue(space.nextMoveId, vl1.x, One)
            val after = space.consult(move)
            assertEq(after.value(costs), False4)
            space.commit(move)
            assertEq(now.value(costs), False4)
        }
        if (true) {
            // move vl1 between r1 and r3 on the left and r2 and r4 on the right
            val move = new ChangeValue(space.nextMoveId, vl1.x, Two)
            val after = space.consult(move)
            assertEq(after.value(costs), True)
            space.commit(move)
            assertEq(now.value(costs), True)
        }
        if (true) {
            // make vl1 intersect r2 and r4
            val move = new ChangeValue(space.nextMoveId, vl1.x, Three)
            val after = space.consult(move)
            assertEq(after.value(costs), False4)
            space.commit(move)
            assertEq(now.value(costs), False4)
        }
        if (true) {
            // move vl1 to the right of r2 and r4
            val move = new ChangeValue(space.nextMoveId, vl1.x, Four)
            val after = space.consult(move)
            assertEq(after.value(costs), True)
            space.commit(move)
            assertEq(now.value(costs), True)
        }
        if (true) {
            // make hl1 intersect r1 and r2
            val move = new ChangeValue(space.nextMoveId, hl1.y, One)
            val after = space.consult(move)
            assertEq(after.value(costs), False4)
            space.commit(move)
            assertEq(now.value(costs), False4)
        }
        if (true) {
            // move hl1 between r1 and r2 below and r3 and r4 above
            val move = new ChangeValue(space.nextMoveId, hl1.y, Two)
            val after = space.consult(move)
            assertEq(after.value(costs), True)
            space.commit(move)
            assertEq(now.value(costs), True)
        }
        if (true) {
            // make hl1 intersect r3 and r4
            val move = new ChangeValue(space.nextMoveId, hl1.y, Three)
            val after = space.consult(move)
            assertEq(after.value(costs), False4)
            space.commit(move)
            assertEq(now.value(costs), False4)
        }
        if (true) {
            // move hl1 above r3 and r4
            val move = new ChangeValue(space.nextMoveId, hl1.y, Four)
            val after = space.consult(move)
            assertEq(after.value(costs), True)
            space.commit(move)
            assertEq(now.value(costs), True)
        }
        if (true) {
            // move p1 into r1
            val move = new ChangeValues(space.nextMoveId, List(new ImmutableEffect(p1.x, One), new ImmutableEffect(p1.y, One)))
            val after = space.consult(move)
            assertEq(after.value(costs), False)
            space.commit(move)
            assertEq(now.value(costs), False)
        }
        if (true) {
            // move p1 to where r1 through r4 meet
            val move = new ChangeValues(space.nextMoveId, List(new ImmutableEffect(p1.x, Two), new ImmutableEffect(p1.y, Two)))
            val after = space.consult(move)
            assertEq(after.value(costs), True)
            space.commit(move)
            assertEq(now.value(costs), True)
        }
        if (true) {
            // let hl2 and vl2 intersect without intersecting other rectangles
            val move = new ChangeValues(space.nextMoveId, List(new ImmutableEffect(hl2.y, Two), new ImmutableEffect(vl2.x, Two)))
            val after = space.consult(move)
            assertEq(after.value(costs), False)
            space.commit(move)
            assertEq(now.value(costs), False)
        }
        if (true) {
            // let hl2 and vl2 intersect each other and other rectangles
            val move = new ChangeValues(space.nextMoveId, List(new ImmutableEffect(hl2.y, Three), new ImmutableEffect(vl2.x, Three)))
            val after = space.consult(move)
            assertEq(after.value(costs), False9)
            space.commit(move)
            assertEq(now.value(costs), False9)
        }
        space.initialize
        assertEq(now.value(costs), False9)
    }

    @Test
    def testRectangleResizingWithStrictSemantics: Unit = {
        val space = new Space(logger)
        val rects = (1 to 3).map(createRect(space, _, new IntegerRange(Zero, Nine)))
        val Vector(r1, r2, r3) = rects
        val costs = new BooleanVariable(space.nextVariableId, "costs", CompleteBooleanDomain)
        space
            .post(new Disjoint2(space.nextConstraintId, null, rects, true, costs))
            .setValue(r1.x, One).setValue(r1.y, One).setValue(r1.w, Three).setValue(r1.h, Three) // fixed rectangle
            .setValue(r2.x, Zero).setValue(r2.y, Zero).setValue(r2.w, Three).setValue(r2.h, One) // does not intersect r1
            .setValue(r3.x, Zero).setValue(r3.y, Zero).setValue(r3.w, One).setValue(r3.h, Three) // intersects r2 in (0, 0)
            .initialize
        val now = space.searchState
        assertEq(now.value(costs), False)
        if (true) {
            // turning r2 into a horizontal line resolves the conflict
            val move = new ChangeValue(space.nextMoveId, r2.h, Zero)
            val after = space.consult(move)
            assertEq(after.value(costs), True)
            space.commit(move)
            assertEq(now.value(costs), True)
        }
        if (true) {
            // turn r3 into a vertical line
            val move = new ChangeValue(space.nextMoveId, r3.w, Zero)
            val after = space.consult(move)
            assertEq(after.value(costs), True)
            space.commit(move)
            assertEq(now.value(costs), True)
        }
        if (true) {
            // moving r1 to (0, 0) does not create a conflict with adjacent lines r2 and r3
            val move = new ChangeValues(space.nextMoveId, List(new ImmutableEffect(r1.x, Zero), new ImmutableEffect(r1.y, Zero)))
            val after = space.consult(move)
            assertEq(after.value(costs), True)
            space.commit(move)
            assertEq(now.value(costs), True)
        }
        if (true) {
            // undoing changes to r2 and r4 creates conflicts
            val move = new ChangeValues(space.nextMoveId, List(new ImmutableEffect(r2.h, One), new ImmutableEffect(r3.w, One)))
            val after = space.consult(move)
            assertEq(after.value(costs), False7)
            space.commit(move)
            assertEq(now.value(costs), False7)
        }
        space.initialize
        assertEq(now.value(costs), False7)
    }

}
