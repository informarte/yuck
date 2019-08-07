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
final class HierarchicalObjectiveTest extends UnitTest {

    private val space = new Space(logger, sigint)
    private val baseDomain = new IntegerRange(Zero, Nine)
    private val x = new IntegerVariable(space.nextVariableId, "x", baseDomain)
    private val y = new IntegerVariable(space.nextVariableId, "y", baseDomain)
    private val mainObjective = new MinimizationObjective(x, Zero, None)
    private val subordinateObjective = new MaximizationObjective(y, Nine, Some(One))
    private val objective = new HierarchicalObjective(List(mainObjective, subordinateObjective), false)

    @Test
    def testCostComparison: Unit = {
        val a = new Assignment
        a.setValue(x, Zero)
        a.setValue(y, One)
        type PLV = PolymorphicListValue
        assertEq(objective.costs(a), new PLV(List(Zero, One)))
        assertEq(objective.compareCosts(new PLV(List(Zero, One)), new PLV(List(Zero, One))), 0)
        assertLt(objective.compareCosts(new PLV(List(Zero, One)), new PLV(List(One, One))), 0)
        assertGt(objective.compareCosts(new PLV(List(One, One)), new PLV(List(Zero, One))), 0)
        assertLt(objective.compareCosts(new PLV(List(Zero, One)), new PLV(List(Zero, Zero))), 0)
        assertGt(objective.compareCosts(new PLV(List(Zero, Zero)), new PLV(List(Zero, One))), 0)
        assertEq(objective.assessMove(a, a), 0)
    }

    @Test
    def testMoveAssessment: Unit = {
        val a = new Assignment
        a.setValue(x, Zero)
        a.setValue(y, One)
        val b = new Assignment
        b.setValue(x, One)
        b.setValue(y, One)
        // check move assessment wrt. main (minimization) goal
        assertEq(objective.assessMove(b, b), 0)
        assertEq(objective.assessMove(a, b), 1)
        assertEq(objective.assessMove(a, b), 1)
        b.setValue(x, Two)
        assertGt(objective.assessMove(a, b), 1)
        objective.assessMove(b, a)
        assertLt(objective.assessMove(b, a), -1)
        // check move assessment wrt. subordinate (maximization) goal
        b.setValue(x, a.value(x))
        a.setValue(y, Zero)
        b.setValue(y, One)
        assertEq(objective.assessMove(b, b), 0)
        assertEq(objective.assessMove(a, b), -1)
        assertEq(objective.assessMove(a, b), -1)
        b.setValue(y, Two)
        assertLt(objective.assessMove(a, b), -1)
        objective.assessMove(b, a)
        assertGt(objective.assessMove(b, a), 1)
    }

    @Test
    def testTightening: Unit = {
        // check tightening of subordinate (maximization) objective
        // starting out from a nearly optimal but infeasible search state
        space
            .post(new DummyConstraint(space.nextConstraintId, List(x, y), Nil))
            .setValue(x, One)
            .setValue(y, Eight)
            .initialize
        if (true) {
            val tighteningResult = objective.tighten(space)
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
            val tighteningResult = objective.tighten(space)
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
            val tighteningResult = objective.tighten(space)
            assert(tighteningResult._1.eq(space.searchState))
            assertEq(tighteningResult._2, None)
        }
    }

}
