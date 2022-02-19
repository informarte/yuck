package yuck.constraints.test

import org.junit.*

import yuck.constraints.*
import yuck.constraints.test.util.ConstraintTestTooling
import yuck.core.*
import yuck.test.util.UnitTest

/**
 * @author Michael Marte
 *
 */
@FixMethodOrder(runners.MethodSorters.NAME_ASCENDING)
final class Disjoint2Test extends UnitTest with ConstraintTestTooling {

    private val space = new Space(logger, sigint)

    private val baseDomain = IntegerRange(0, 9)
    private val costs = new BooleanVariable(space.nextVariableId(), "costs", CompleteBooleanDomain)

    private def createRect(i: Int): Disjoint2Rect =
        new Disjoint2Rect(
            new IntegerVariable(space.nextVariableId(), "x%d".format(i), baseDomain),
            new IntegerVariable(space.nextVariableId(), "y%d".format(i), baseDomain),
            new IntegerVariable(space.nextVariableId(), "w%d".format(i), baseDomain),
            new IntegerVariable(space.nextVariableId(), "h%d".format(i), baseDomain))

    @Test
    def testBasics(): Unit = {
        val rects = (1 to 2).map(createRect)
        val Seq(r1, r2) = rects
        val constraint = new Disjoint2(space.nextConstraintId(), null, rects, false, costs)
        assertEq(constraint.toString, "disjoint2([(x1, y1, w1, h1), (x2, y2, w2, h2)], costs)")
        assertEq(constraint.inVariables.size, 8)
        assertEq(constraint.inVariables.toSet, Set(r1.x, r1.y, r1.w, r1.h, r2.x, r2.y, r2.w, r2.h))
        assertEq(constraint.outVariables.size, 1)
        assertEq(constraint.outVariables.head, costs)
    }

    @Test
    def testRectangleMovement(): Unit = {
        val rects = (1 to 4).map(createRect)
        val Seq(r1, r2, r3, r4) = rects
        space.post(new Disjoint2(space.nextConstraintId(), null, rects, false, costs))
        runScenario(
            TestScenario(
                space,
                Initialize(
                    "initial conflict: r1 and r3 are identical, r3 overlaps with r1 and r2",
                    r1.x << 0, r1.y << 1, r1.w << 3, r1.h << 4,
                    r2.x << 5, r2.y << 4, r2.w << 3, r2.h << 2, // disjoint from r1
                    r3.x << 0, r3.y << 1, r3.w << 3, r3.h << 4, // identical to r1
                    r4.x << 2, r4.y << 3, r4.w << 5, r4.h << 4, // overlaps r1, r2, and r3
                    costs << BooleanValue(20)),
                ConsultAndCommit("move r1 from (0, 1) to (3, 1)", r1.x << 3, costs << BooleanValue(13)),
                ConsultAndCommit("move r3 from (0, 1) to (0, 3)", r3.y << 3, costs << BooleanValue(15)),
                ConsultAndCommit("move r1 and r3 back", r1.x << 0, r3.y << 1, costs << BooleanValue(20))))
    }

    @Test
    def testRectangleResizing(): Unit = {
        val rects = (1 to 2).map(createRect)
        val Seq(r1, r2) = rects
        space.post(new Disjoint2(space.nextConstraintId(), null, rects, false, costs))
        runScenario(
            TestScenario(
                space,
                Initialize(
                    "initial conflict: r2 overlaps with r1",
                    r1.x << 0, r1.y << 1, r1.w << 4, r1.h << 4,
                    r2.x << 2, r2.y << 3, r2.w << 4, r2.h << 4,
                    costs << False4),
                ConsultAndCommit("reduce height and width of r2 to 1", r2.w << 1, r2.h << 1, costs << False),
                ConsultAndCommit("restore width of r2", r2.w << 4, costs << False2),
                ConsultAndCommit("restore height of r2", r2.h << 4, costs << False4)))
    }

    @Test
    def testSimultaneousRectangleMovementAndResizing(): Unit = {
        val rects = (1 to 2).map(createRect)
        val Seq(r1, r2) = rects
        space.post(new Disjoint2(space.nextConstraintId(), null, rects, false, costs))
        runScenario(
            TestScenario(
                space,
                Initialize(
                    "no initial conflict",
                    r1.x << 0, r1.y << 0, r1.w << 4, r1.h << 4,
                    r2.x << 4, r2.y << 4, r2.w << 4, r2.h << 4,
                    costs << True),
                ConsultAndCommit(
                    "move and resize r1 to partially overlap r2",
                    r1.x << 2, r1.y << 3, r1.w << 3, r1.h << 2,
                    costs << False),
                ConsultAndCommit("move r2 and resize r1", r1.w << 4, r2.x << 3, r2.y << 2, costs << False6)))
    }

    @Test
    def testHandlingOfAdjacentRectangles(): Unit = {
        val rects = (1 to 3).map(createRect)
        val Seq(r1, r2, r3) = rects
        space.post(new Disjoint2(space.nextConstraintId(), null, rects, false, costs))
        runScenario(
            TestScenario(
                space,
                Initialize(
                    "r2 is adjacent to r1 and r3 is adjacent to r1 and r2",
                    r1.x << 0, r1.y << 0, r1.w << 1, r1.h << 1,
                    r2.x << 0, r2.y << 1, r2.w << 1, r2.h << 1,
                    r3.x << 1, r3.y << 0, r3.w << 1, r3.h << 2,
                    costs << True)))
    }

    @Test
    def testHandlingOfSharedVariables(): Unit = {
        val r1 = createRect(1)
        val r2 =
            new Disjoint2Rect(
                r1.x,
                new IntegerVariable(space.nextVariableId(), "y2", baseDomain),
                r1.w,
                new IntegerVariable(space.nextVariableId(), "h2", baseDomain))
        val r3 = createRect(3)
        space.post(new Disjoint2(space.nextConstraintId(), null, Vector(r1, r2, r3, r1), false, costs))
        runScenario(
            TestScenario(
                space,
                Initialize(
                    "initial conflict because r1 occurs twice",
                    r1.x << 0, r1.y << 0, r1.w << 2, r1.h << 2,
                    r2.y << 2, r2.h << 2, // adjacent to r1
                    r3.x << 2, r3.y << 0, r3.w << 2, r3.h << 4, // adjacent to r1 and r2
                    costs << False4),
                ConsultAndCommit("move r1 and r2 to partially overlap r3", r1.x << 1, costs << False10),
                ConsultAndCommit("make r1 and r2 wider to fully overlap r3", r1.w << 4, costs << BooleanValue(20))))
    }

    @Test
    def testConsultWithoutCommit(): Unit = {
        val rects = (1 to 2).map(createRect)
        val Seq(r1, r2) = rects
        space.post(new Disjoint2(space.nextConstraintId(), null, rects, false, costs))
        runScenario(
            TestScenario(
                space,
                Initialize(
                    "initial conflict: r2 overlaps with r1",
                    r1.x << 0, r1.y << 1, r1.w << 4, r1.h << 4,
                    r2.x << 2, r2.y << 3, r2.w << 4, r2.h << 4,
                    costs << False4),
                Consult("reduce height and width of r2 to 1", r2.w << 1, r2.h << 1, costs << False),
                Consult("move r1 to fully overlap r2", r1.x << 2, r1.y << 3, costs << BooleanValue(16))))
    }

    @Test
    def testNonStrictSemantics(): Unit = {
        val rects = (1 to 2).map(createRect)
        val Seq(r1, r2) = rects
        space.post(new Disjoint2(space.nextConstraintId(), null, rects, false, costs))
        runScenario(
            TestScenario(
                space,
                Initialize(
                    "initial conflict: r2 overlaps with r1",
                    r1.x << 0, r1.y << 1, r1.w << 4, r1.h << 4,
                    r2.x << 2, r2.y << 3, r2.w << 4, r2.h << 4,
                    costs << False4),
                ConsultAndCommit("reduce width of r2 to 0", r2.w << 0, costs << True),
                ConsultAndCommit("restore width of r2", r2.w << 4, costs << False4)))
    }

    @Test
    def testRectangleMovementWithStrictSemantics(): Unit = {
        val rects = (1 to 9).map(createRect)
        val Seq(r1, r2, r3, r4, vl1, vl2, hl1, hl2, p1) = rects
        space.post(new Disjoint2(space.nextConstraintId(), null, rects, true, costs))
        runScenario(
            TestScenario(
                space,
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
                Initialize(
                    "setup",
                    // r1 through r4 are adjacent rectangles
                    r1.x << 0, r1.y << 0, r1.w << 2, r1.h << 2,
                    r2.x << 2, r2.y << 0, r2.w << 2, r2.h << 2,
                    r3.x << 0, r3.y << 2, r3.w << 2, r3.h << 2,
                    r4.x << 2, r4.y << 2, r4.w << 2, r4.h << 2,
                    // vl1 and vl2 are vertical lines
                    vl1.x << 0, vl1.y << 0, vl1.w << 0, vl1.h << 4,
                    vl2.x << 0, vl2.y << 0, vl2.w << 0, vl2.h << 4,
                    // hl1 and hl2 are horizontal lines
                    hl1.x << 0, hl1.y << 0, hl1.w << 4, hl1.h << 0,
                    hl2.x << 0, hl2.y << 0, hl2.w << 4, hl2.h << 0,
                    // p1 is a point
                    p1.x << 0, p1.y << 0, p1.w << 0, p1.h << 0,
                    costs << True),
                ConsultAndCommit("make vl1 intersect r1 and r3", vl1.x << 1, costs << False4),
                ConsultAndCommit("move vl1 between r1 and r3 on the left and r2 and r4 on the right", vl1.x << 2, costs << True),
                ConsultAndCommit("make vl1 intersect r2 and r4", vl1.x << 3, costs << False4),
                ConsultAndCommit("move vl1 to the right of r2 and r4", vl1.x << 4, costs << True),
                ConsultAndCommit("make hl1 intersect r1 and r2", hl1.y << 1, costs << False4),
                ConsultAndCommit("move hl1 between r1 and r2 below and r3 and r4 above", hl1.y << 2, costs << True),
                ConsultAndCommit("make hl1 intersect r3 and r4", hl1.y << 3, costs << False4),
                ConsultAndCommit("move hl1 above r3 and r4", hl1.y << 4, costs << True),
                ConsultAndCommit("move p1 into r1", p1.x << 1, p1.y << 1, costs << False),
                ConsultAndCommit("move p1 to where r1 through r4 meet", p1.x << 2, p1.y << 2, costs << True),
                ConsultAndCommit("let hl2 and vl2 intersect without intersecting other rectangles", hl2.y << 2, vl2.x << 2, costs << False),
                ConsultAndCommit("let hl2 and vl2 intersect each other and other rectangles", hl2.y << 3, vl2.x << 3, costs << False9)))
    }

    @Test
    def testRectangleResizingWithStrictSemantics(): Unit = {
        val rects = (1 to 3).map(createRect)
        val Seq(r1, r2, r3) = rects
        space.post(new Disjoint2(space.nextConstraintId(), null, rects, true, costs))
        runScenario(
            TestScenario(
                space,
                Initialize(
                    "setup",
                    r1.x << 1, r1.y << 1, r1.w << 3, r1.h << 3, // fixed rectangle
                    r2.x << 0, r2.y << 0, r2.w << 3, r2.h << 1, // does not intersect r1
                    r3.x << 0, r3.y << 0, r3.w << 1, r3.h << 3, // intersects r2 in (0, 0)
                    costs << False),
                ConsultAndCommit("turning r2 into a horizontal line resolves the conflict", r2.h << 0, costs << True),
                ConsultAndCommit("turn r3 into a vertical line", r3.w << 0, costs << True),
                ConsultAndCommit(
                    "moving r1 to (0, 0) does not create a conflict with adjacent lines r2 and r3",
                    r1.x << 0, r1.y << 0,
                    costs << True),
                ConsultAndCommit("undoing changes to r2 and r4 creates conflicts", r2.h << 1, r3.w << 1, costs << False7)))
    }

    @Test
    def testHandlingOfNegativeWidthAndHeight(): Unit = {
        val rects = (1 to 3).map(createRect)
        val Seq(r1, r2, r3) = rects
        space.post(new Disjoint2(space.nextConstraintId(), null, rects, true, costs))
        runScenario(
            TestScenario(
                space,
                Initialize(
                    "initial conflict: r1 and r2 are identical and covered by r0",
                    r1.x << 0, r1.y << 0, r1.w << 3, r1.h << 3,
                    r2.x << 0, r2.y << 0, r2.w << 1, r2.h << 1,
                    r3.x << 0, r3.y << 0, r3.w << 1, r3.h << 1,
                    costs << False3),
                ConsultAndCommit("reduce conflict by setting the width of r2 to -1", r2.w << -1, costs << False),
                ConsultAndCommit("resolve conflict by setting the height of r3 to -1", r3.h << -1, costs << True)))
    }

}
