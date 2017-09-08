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
final class ObjectiveTest extends UnitTest {

    @Test
    def testMinimizationObjective {
        val space = new Space(logger)
        val d = new IntegerDomain(Zero, Nine)
        val x = space.createVariable("x", d)
        val a = new Assignment
        a.setValue(x, Zero)
        val o = new MinimizationObjective(x, Zero, Some(MinusOne))
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
        // check tightening
        space
        .post(new DummyConstraint(space.constraintIdFactory.nextId, List(x), Nil))
        .setValue(x, One)
        .initialize
        // check that tightening finds lower bound of x
        if (true) {
            val tighteningResult = o.tighten(space)
            assert(! tighteningResult._1.eq(space.searchState))
            assertEq(tighteningResult._1.mappedVariables, Set(x))
            assertEq(tighteningResult._1.value(x), Zero)
            assertEq(tighteningResult._2, Some(x))
            assertEq(space.searchState.value(x), Zero)
            assert(x.domain.isSingleton)
            assertEq(x.domain.singleValue, Zero)
        }
        // check that further tightening is not possible
        if (true) {
            val tighteningResult = o.tighten(space)
            assert(tighteningResult._1.eq(space.searchState))
            assertEq(tighteningResult._2, None)
        }
    }

    @Test
    def testMaximizationObjective {
        val space = new Space(logger)
        val d = new IntegerDomain(Zero, Nine)
        val x = space.createVariable("x", d)
        val a = new Assignment
        a.setValue(x, Zero)
        val o = new MaximizationObjective(x, Zero, Some(One))
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
        // check tightening
        space
        .post(new DummyConstraint(space.constraintIdFactory.nextId, List(x), Nil))
        .setValue(x, Eight)
        .initialize
        // check that tightening finds upper bound of x
        if (true) {
          val tighteningResult = o.tighten(space)
          assert(! tighteningResult._1.eq(space.searchState))
          assertEq(tighteningResult._1.mappedVariables, Set(x))
          assertEq(tighteningResult._1.value(x), Nine)
          assertEq(tighteningResult._2, Some(x))
          assertEq(space.searchState.value(x), Nine)
          assert(x.domain.isSingleton)
          assertEq(x.domain.singleValue, Nine)
        }
        // check that further tightening is not possible
        if (true) {
            val tighteningResult = o.tighten(space)
            assert(tighteningResult._1.eq(space.searchState))
            assertEq(tighteningResult._2, None)
        }
    }

    @Test
    def testHierarchicalObjective {
        val space = new Space(logger)
        val d = new IntegerDomain(Zero, Nine)
        val x = space.createVariable("x", d)
        val y = space.createVariable("y", d)
        val a = new Assignment
        a.setValue(x, Zero)
        a.setValue(y, One)
        val o = new HierarchicalObjective(List(new MinimizationObjective(x, Zero, None), new MaximizationObjective(y, Nine, Some(One))), false)
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
        // check tightening of subordinate (maximization) objective
        // starting out from a nearly optimal but infeasible search state
        space
        .post(new DummyConstraint(space.constraintIdFactory.nextId, List(x, y), Nil))
        .setValue(x, One)
        .setValue(y, Eight)
        .initialize
        if (true) {
            val tighteningResult = o.tighten(space)
            assert(tighteningResult._1.eq(space.searchState))
            assertEq(tighteningResult._2, None)
            assertEq(space.searchState.value(x), One)
            assertEq(space.searchState.value(y), Eight)
            assertEq(x.domain.size, 10)
            assertEq(y.domain.size, 10)
        }
        // now move to a solution to facilitate tightening of y
        if (true) {
            space.setValue(x, Zero).initialize
            val tighteningResult = o.tighten(space)
            assert(! tighteningResult._1.eq(space.searchState))
            assertEq(tighteningResult._1.mappedVariables, Set(x, y))
            assertEq(tighteningResult._1.value(x), Zero)
            assertEq(tighteningResult._1.value(y), Nine)
            assertEq(tighteningResult._2, Some(y))
            assertEq(space.searchState.value(x), Zero)
            assertEq(space.searchState.value(y), Nine)
            assertEq(x.domain.size, 10)
            assertEq(y.domain.size, 1)
        }
        // check that further tightening is not possible
        if (true) {
            val tighteningResult = o.tighten(space)
            assert(tighteningResult._1.eq(space.searchState))
            assertEq(tighteningResult._2, None)
        }
    }

}
