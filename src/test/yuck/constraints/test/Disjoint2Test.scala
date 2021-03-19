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
@FixMethodOrder(runners.MethodSorters.NAME_ASCENDING)
final class Disjoint2Test extends UnitTest with AssignmentPropagationTestTooling {

    private def createRect(space: Space, i: Int, d: IntegerDomain): Disjoint2Rect =
        new Disjoint2Rect(
            new IntegerVariable(space.nextVariableId, "x%d".format(i), d),
            new IntegerVariable(space.nextVariableId, "y%d".format(i), d),
            new IntegerVariable(space.nextVariableId, "w%d".format(i), d),
            new IntegerVariable(space.nextVariableId, "h%d".format(i), d))

    @Test
    def testSearchVariables: Unit = {
        val space = new Space(logger, sigint)
        val rects = (1 to 2).map(createRect(space, _, IntegerRange(Zero, Nine)))
        val Vector(r1, r2) = rects
        val costs = new BooleanVariable(space.nextVariableId, "costs", CompleteBooleanDomain)
        space.post(new Disjoint2(space.nextConstraintId, null, rects, false, costs))
        assertEq(space.searchVariables, Set(r1.x, r1.y, r1.w, r1.h, r2.x, r2.y, r2.w, r2.h))
    }

    @Test
    def testRectangleMovement: Unit = {
        val space = new Space(logger, sigint)
        val rects = (1 to 4).map(createRect(space, _, IntegerRange(Zero, Nine)))
        val Vector(r1, r2, r3, r4) = rects
        val costs = new BooleanVariable(space.nextVariableId, "costs", CompleteBooleanDomain)
        space.post(new Disjoint2(space.nextConstraintId, null, rects, false, costs))
        runScenario(
            TestScenario(
                space,
                Initialize(
                    "initial conflict: r1 and r3 are identical, r3 overlaps with r1 and r2",
                    (r1.x, Zero), (r1.y, One), (r1.w, Three), (r1.h, Four),
                    (r2.x, Five), (r2.y, Four), (r2.w, Three), (r2.h, Two), // disjoint from r1
                    (r3.x, Zero), (r3.y, One), (r3.w, Three), (r3.h, Four), // identical to r1
                    (r4.x, Two), (r4.y, Three), (r4.w, Five), (r4.h, Four), // overlaps r1, r2, and r3
                    (costs, BooleanValue(20))),
                ConsultAndCommit("move r1 from (0, 1) to (3, 1)", (r1.x, Three), (costs, BooleanValue(13))),
                ConsultAndCommit("move r3 from (0, 1) to (0, 3)", (r3.y, Three), (costs, BooleanValue(15))),
                ConsultAndCommit("move r1 and r3 back", (r1.x, Zero), (r3.y, One), (costs, BooleanValue(20)))))
    }

    @Test
    def testRectangleResizing: Unit = {
        val space = new Space(logger, sigint)
        val rects = (1 to 2).map(createRect(space, _, IntegerRange(Zero, Nine)))
        val Vector(r1, r2) = rects
        val costs = new BooleanVariable(space.nextVariableId, "costs", CompleteBooleanDomain)
        space.post(new Disjoint2(space.nextConstraintId, null, rects, false, costs))
        runScenario(
            TestScenario(
                space,
                Initialize(
                    "initial conflict: r2 overlaps with r1",
                    (r1.x, Zero), (r1.y, One), (r1.w, Four), (r1.h, Four),
                    (r2.x, Two), (r2.y, Three), (r2.w, Four), (r2.h, Four),
                    (costs, False4)),
                ConsultAndCommit("reduce height and width of r2 to 1", (r2.w, One), (r2.h, One), (costs, False)),
                ConsultAndCommit("restore width of r2", (r2.w, Four), (costs, False2)),
                ConsultAndCommit("restore height of r2", (r2.h, Four), (costs, False4))))
    }

    @Test
    def testSimultaneousRectangleMovementAndResizing: Unit = {
        val space = new Space(logger, sigint)
        val rects = (1 to 2).map(createRect(space, _, IntegerRange(Zero, Nine)))
        val Vector(r1, r2) = rects
        val costs = new BooleanVariable(space.nextVariableId, "costs", CompleteBooleanDomain)
        space.post(new Disjoint2(space.nextConstraintId, null, rects, false, costs))
        runScenario(
            TestScenario(
                space,
                Initialize(
                    "no initial conflict",
                    (r1.x, Zero), (r1.y, Zero), (r1.w, Four), (r1.h, Four),
                    (r2.x, Four), (r2.y, Four), (r2.w, Four), (r2.h, Four),
                    (costs, True)),
                ConsultAndCommit(
                    "move and resize r1 to partially overlap r2",
                    (r1.x, Two), (r1.y, Three), (r1.w, Three), (r1.h, Two),
                    (costs, False)),
                ConsultAndCommit("move r2 and resize r1", (r1.w, Four), (r2.x, Three), (r2.y, Two), (costs, False6))))
    }

    @Test
    def testHandlingOfAdjacentRectangles: Unit = {
        val space = new Space(logger, sigint)
        val rects = (1 to 3).map(createRect(space, _, IntegerRange(Zero, Nine)))
        val Vector(r1, r2, r3) = rects
        val costs = new BooleanVariable(space.nextVariableId, "costs", CompleteBooleanDomain)
        space.post(new Disjoint2(space.nextConstraintId, null, rects, false, costs))
        runScenario(
            TestScenario(
                space,
                Initialize(
                    "r2 is adjacent to r1 and r3 is adjacent to r1 and r2",
                    (r1.x, Zero), (r1.y, Zero), (r1.w, One), (r1.h, One),
                    (r2.x, Zero), (r2.y, One), (r2.w, One), (r2.h, One),
                    (r3.x, One), (r3.y, Zero), (r3.w, One), (r3.h, Two),
                    (costs, True))))
    }

    @Test
    def testHandlingOfSharedVariables: Unit = {
        val space = new Space(logger, sigint)
        val d = IntegerRange(Zero, Nine)
        val r1 = createRect(space, 1, d)
        val r2 =
            new Disjoint2Rect(
                r1.x,
                new IntegerVariable(space.nextVariableId, "y2", d),
                r1.w,
                new IntegerVariable(space.nextVariableId, "h2", d))
        val r3 = createRect(space, 3, d)
        val costs = new BooleanVariable(space.nextVariableId, "costs", CompleteBooleanDomain)
        space.post(new Disjoint2(space.nextConstraintId, null, Vector(r1, r2, r3, r1), false, costs))
        runScenario(
            TestScenario(
                space,
                Initialize(
                    "initial conflict because r1 occurs twice",
                    (r1.x, Zero), (r1.y, Zero), (r1.w, Two), (r1.h, Two),
                    (r2.y, Two), (r2.h, Two), // adjacent to r1
                    (r3.x, Two), (r3.y, Zero), (r3.w, Two), (r3.h, Four), // adjacent to r1 and r2
                    (costs, False4)),
                ConsultAndCommit("move r1 and r2 to partially overlap r3", (r1.x, One), (costs, False10)),
                ConsultAndCommit("make r1 and r2 wider to fully overlap r3", (r1.w, Four), (costs, BooleanValue(20)))))
    }

    @Test
    def testConsultWithoutCommit: Unit = {
        val space = new Space(logger, sigint)
        val rects = (1 to 2).map(createRect(space, _, IntegerRange(Zero, Nine)))
        val Vector(r1, r2) = rects
        val costs = new BooleanVariable(space.nextVariableId, "costs", CompleteBooleanDomain)
        space.post(new Disjoint2(space.nextConstraintId, null, rects, false, costs))
        runScenario(
            TestScenario(
                space,
                Initialize(
                    "initial conflict: r2 overlaps with r1",
                    (r1.x, Zero), (r1.y, One), (r1.w, Four), (r1.h, Four),
                    (r2.x, Two), (r2.y, Three), (r2.w, Four), (r2.h, Four),
                    (costs, False4)),
                Consult("reduce height and width of r2 to 1", (r2.w, One), (r2.h, One), (costs, False)),
                Consult("move r1 to fully overlap r2", (r1.x, Two), (r1.y, Three), (costs, BooleanValue(16)))))
    }

    @Test
    def testNonStrictSemantics: Unit = {
        val space = new Space(logger, sigint)
        val rects = (1 to 2).map(createRect(space, _, IntegerRange(Zero, Nine)))
        val Vector(r1, r2) = rects
        val costs = new BooleanVariable(space.nextVariableId, "costs", CompleteBooleanDomain)
        space.post(new Disjoint2(space.nextConstraintId, null, rects, false, costs))
        runScenario(
            TestScenario(
                space,
                Initialize(
                    "initial conflict: r2 overlaps with r1",
                    (r1.x, Zero), (r1.y, One), (r1.w, Four), (r1.h, Four),
                    (r2.x, Two), (r2.y, Three), (r2.w, Four), (r2.h, Four),
                    (costs, False4)),
                ConsultAndCommit("reduce width of r2 to 0", (r2.w, Zero), (costs, True)),
                ConsultAndCommit("restore width of r2", (r2.w, Four), (costs, False4))))
    }

    @Test
    def testRectangleMovementWithStrictSemantics: Unit = {
        val space = new Space(logger, sigint)
        val rects = (1 to 9).map(createRect(space, _, IntegerRange(Zero, Nine)))
        val Vector(r1, r2, r3, r4, vl1, vl2, hl1, hl2, p1) = rects
        val costs = new BooleanVariable(space.nextVariableId, "costs", CompleteBooleanDomain)
        space.post(new Disjoint2(space.nextConstraintId, null, rects, true, costs))
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
                    (r1.x, Zero), (r1.y, Zero), (r1.w, Two), (r1.h, Two),
                    (r2.x, Two), (r2.y, Zero), (r2.w, Two), (r2.h, Two),
                    (r3.x, Zero), (r3.y, Two), (r3.w, Two), (r3.h, Two),
                    (r4.x, Two), (r4.y, Two), (r4.w, Two), (r4.h, Two),
                    // vl1 and vl2 are vertical lines
                    (vl1.x, Zero), (vl1.y, Zero), (vl1.w, Zero), (vl1.h, Four),
                    (vl2.x, Zero), (vl2.y, Zero), (vl2.w, Zero), (vl2.h, Four),
                    // hl1 and hl2 are horizontal lines
                    (hl1.x, Zero), (hl1.y, Zero), (hl1.w, Four), (hl1.h, Zero),
                    (hl2.x, Zero), (hl2.y, Zero), (hl2.w, Four), (hl2.h, Zero),
                    // p1 is a point
                    (p1.x, Zero), (p1.y, Zero), (p1.w, Zero), (p1.h, Zero),
                    (costs, True)),
                ConsultAndCommit("make vl1 intersect r1 and r3", (vl1.x, One), (costs, False4)),
                ConsultAndCommit("move vl1 between r1 and r3 on the left and r2 and r4 on the right", (vl1.x, Two), (costs, True)),
                ConsultAndCommit("make vl1 intersect r2 and r4", (vl1.x, Three), (costs, False4)),
                ConsultAndCommit("move vl1 to the right of r2 and r4", (vl1.x, Four), (costs, True)),
                ConsultAndCommit("make hl1 intersect r1 and r2", (hl1.y, One), (costs, False4)),
                ConsultAndCommit("move hl1 between r1 and r2 below and r3 and r4 above", (hl1.y, Two), (costs, True)),
                ConsultAndCommit("make hl1 intersect r3 and r4", (hl1.y, Three), (costs, False4)),
                ConsultAndCommit("move hl1 above r3 and r4", (hl1.y, Four), (costs, True)),
                ConsultAndCommit("move p1 into r1", (p1.x, One), (p1.y, One), (costs, False)),
                ConsultAndCommit("move p1 to where r1 through r4 meet", (p1.x, Two), (p1.y, Two), (costs, True)),
                ConsultAndCommit("let hl2 and vl2 intersect without intersecting other rectangles", (hl2.y, Two), (vl2.x, Two), (costs, False)),
                ConsultAndCommit("let hl2 and vl2 intersect each other and other rectangles", (hl2.y, Three), (vl2.x, Three), (costs, False9))))
    }

    @Test
    def testRectangleResizingWithStrictSemantics: Unit = {
        val space = new Space(logger, sigint)
        val rects = (1 to 3).map(createRect(space, _, IntegerRange(Zero, Nine)))
        val Vector(r1, r2, r3) = rects
        val costs = new BooleanVariable(space.nextVariableId, "costs", CompleteBooleanDomain)
        space.post(new Disjoint2(space.nextConstraintId, null, rects, true, costs))
        runScenario(
            TestScenario(
                space,
                Initialize(
                    "setup",
                    (r1.x, One), (r1.y, One), (r1.w, Three), (r1.h, Three), // fixed rectangle
                    (r2.x, Zero), (r2.y, Zero), (r2.w, Three), (r2.h, One), // does not intersect r1
                    (r3.x, Zero), (r3.y, Zero), (r3.w, One), (r3.h, Three), // intersects r2 in (0, 0)
                    (costs, False)),
                ConsultAndCommit("turning r2 into a horizontal line resolves the conflict", (r2.h, Zero), (costs, True)),
                ConsultAndCommit("turn r3 into a vertical line", (r3.w, Zero), (costs, True)),
                ConsultAndCommit(
                    "moving r1 to (0, 0) does not create a conflict with adjacent lines r2 and r3",
                    (r1.x, Zero), (r1.y, Zero),
                    (costs, True)),
                ConsultAndCommit("undoing changes to r2 and r4 creates conflicts", (r2.h, One), (r3.w, One), (costs, False7))))
    }

    @Test
    def testHandlingOfNegativeWidthAndHeight: Unit = {
        val space = new Space(logger, sigint)
        val rects = (1 to 3).map(createRect(space, _, IntegerRange(Zero, Nine)))
        val Vector(r1, r2, r3) = rects
        val costs = new BooleanVariable(space.nextVariableId, "costs", CompleteBooleanDomain)
        space.post(new Disjoint2(space.nextConstraintId, null, rects, true, costs))
        runScenario(
            TestScenario(
                space,
                Initialize(
                    "initial conflict: r1 and r2 are identical and covered by r0",
                    (r1.x, Zero), (r1.y, Zero), (r1.w, Three), (r1.h, Three),
                    (r2.x, Zero), (r2.y, Zero), (r2.w, One), (r2.h, One),
                    (r3.x, Zero), (r3.y, Zero), (r3.w, One), (r3.h, One),
                    (costs, False3)),
                ConsultAndCommit("reduce conflict by setting the width of r2 to -1", (r2.w, MinusOne), (costs, False)),
                ConsultAndCommit("resolve conflict by setting the height of r3 to -1", (r3.h, MinusOne), (costs, True))))
    }

}
