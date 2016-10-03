package yuck.core.test

import org.junit._
import yuck.core._
import yuck.util.testing.UnitTest

/**
 * @author Michael Marte
 *
 */
@Test
@FixMethodOrder(runners.MethodSorters.NAME_ASCENDING)
class ObjectiveTest extends UnitTest {

    @Test
    def testMinimizationObjective {
        val space = new Space
        val d = new IntegerDomain(Zero, Nine)
        val x = space.createVariable("x", d)
        val a = new Assignment
        a.setValue(x, Zero)
        val o = new MinimizationObjective(x, Zero)
        // check cost comparison
        assertEq(o.costs(a), Zero)
        assertEq(o.compareCosts(Zero, Zero), 0)
        assertLt(o.compareCosts(Zero, One), 0)
        assertGt(o.compareCosts(One, Zero), 0)
        assertEq(o.assessMove(a, a), 0)
        // check move assessment
        val b = new Assignment
        b.setValue(x, One)
        assertEq(o.assessMove(b, b), 0)
        assertEq(o.assessMove(a, b), 1)
        assertEq(o.assessMove(a, b), 1)
        b.setValue(x, Two)
        assertGt(o.assessMove(a, b), 1)
        b.setValue(x, Three)
        assertGt(o.assessMove(a, b), 1)
        o.assessMove(b, a)
        assertLt(o.assessMove(b, a), -1)
    }

    @Test
    def testMaximizationObjective {
        val space = new Space
        val d = new IntegerDomain(Zero, Nine)
        val x = space.createVariable("x", d)
        val a = new Assignment
        a.setValue(x, Zero)
        val o = new MaximizationObjective(x, Zero)
        // check cost comparison
        assertEq(o.costs(a), Zero)
        assertEq(o.compareCosts(Zero, Zero), 0)
        assertGt(o.compareCosts(Zero, One), 0)
        assertLt(o.compareCosts(One, Zero), 0)
        assertEq(o.assessMove(a, a), 0)
        // check move assessment
        val b = new Assignment
        b.setValue(x, One)
        assertEq(o.assessMove(b, b), 0)
        assertEq(o.assessMove(a, b), -1)
        assertEq(o.assessMove(a, b), -1)
        b.setValue(x, Two)
        assertLt(o.assessMove(a, b), -1)
        o.assessMove(b, a)
        assertGt(o.assessMove(b, a), 1)
    }

    @Test
    def testHierarchicalObjective {
        val space = new Space
        val d = new IntegerDomain(Zero, Nine)
        val x = space.createVariable("x", d)
        val y = space.createVariable("y", d)
        val a = new Assignment
        a.setValue(x, Zero)
        a.setValue(y, One)
        val o = new HierarchicalObjective(List(new MinimizationObjective(x, Zero), new MaximizationObjective(y, null)), false)
        type PLV = PolymorphicListValue
        // check cost comparison
        assertEq(o.costs(a), new PLV(List(Zero, One)))
        assertEq(o.compareCosts(new PLV(List(Zero, One)), new PLV(List(Zero, One))), 0)
        assertLt(o.compareCosts(new PLV(List(Zero, One)), new PLV(List(One, One))), 0)
        assertGt(o.compareCosts(new PLV(List(One, One)), new PLV(List(Zero, One))), 0)
        assertLt(o.compareCosts(new PLV(List(Zero, One)), new PLV(List(Zero, Zero))), 0)
        assertGt(o.compareCosts(new PLV(List(Zero, Zero)), new PLV(List(Zero, One))), 0)
        assertEq(o.assessMove(a, a), 0)
        // check move assessment wrt. main (minimization) goal
        val b = new Assignment
        b.setValue(x, One)
        b.setValue(y, One)
        assertEq(o.assessMove(b, b), 0)
        assertEq(o.assessMove(a, b), 1)
        assertEq(o.assessMove(a, b), 1)
        b.setValue(x, Two)
        assertGt(o.assessMove(a, b), 1)
        o.assessMove(b, a)
        assertLt(o.assessMove(b, a), -1)
        // check move assessment wrt. subordinate (maximization) goal
        b.setValue(x, a.value(x))
        a.setValue(y, Zero)
        b.setValue(y, One)
        assertEq(o.assessMove(b, b), 0)
        assertEq(o.assessMove(a, b), -1)
        assertEq(o.assessMove(a, b), -1)
        b.setValue(y, Two)
        assertLt(o.assessMove(a, b), -1)
        o.assessMove(b, a)
        assertGt(o.assessMove(b, a), 1)
    }

}
