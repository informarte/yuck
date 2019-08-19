package yuck.core.test

import org.junit._

import yuck.constraints.Lt
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

    @Test
    def testCostComparison: Unit = {
        val mainObjective = new MinimizationObjective(x, Zero, None)
        val subordinateObjective = new MaximizationObjective(y, Nine, Some(One))
        val objective = new HierarchicalObjective(List(mainObjective, subordinateObjective), false)
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
        import scala.math.Ordering.Double.TotalOrdering
        val mainObjective = new MinimizationObjective(x, Zero, None)
        val subordinateObjective = new MaximizationObjective(y, Nine, Some(One))
        val objective = new HierarchicalObjective(List(mainObjective, subordinateObjective), false)
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
    def testTighteningWithMinimization: Unit = {
        val z = new BooleanVariable(space.nextVariableId, "z", TrueDomain)
        val mainObjective = new MinimizationObjective(z, True, None)
        val subordinateObjective = new MinimizationObjective(y, baseDomain.lb, Some(MinusOne))
        val objective = new HierarchicalObjective(List(mainObjective, subordinateObjective), false)
        val now = space.searchState
        space.post(new Lt(space.nextConstraintId, null, x, y, z))
        for (a <- x.domain.values; b <- y.domain.values) {
            space.setValue(x, a).setValue(y, b).initialize
            assertEq(now.value(z).truthValue, a < b)
            val TighteningResult(tightenedState, maybeTightenedVariable) = objective.tighten(space)
            assertEq(tightenedState.mappedVariables, Set(x, y, z))
            if (a < b) {
                assertEq(tightenedState.value(x), a)
                assertEq(tightenedState.value(y), a + One)
                assertEq(maybeTightenedVariable, Some(y))
                assertEq(x.domain, baseDomain)
                assertEq(y.domain.ub, a)
                assertEq(now.value(x), a)
                assertEq(now.value(y), a)
            } else {
                assertEq(tightenedState.value(x), a)
                assertEq(tightenedState.value(y), b)
                assertEq(maybeTightenedVariable, None)
                assertEq(x.domain, baseDomain)
                assertEq(y.domain, baseDomain)
                assertEq(now.value(x), a)
                assertEq(now.value(y), b)
            }
            assertEq(now.value(z).truthValue, false)
            x.relaxDomain(baseDomain)
            y.relaxDomain(baseDomain)
        }
    }

    @Test
    def testTighteningWithMaximization: Unit = {
        val z = new BooleanVariable(space.nextVariableId, "z", TrueDomain)
        val mainObjective = new MinimizationObjective(z, True, None)
        val subordinateObjective = new MaximizationObjective(y, baseDomain.ub, Some(One))
        val objective = new HierarchicalObjective(List(mainObjective, subordinateObjective), false)
        val now = space.searchState
        space.post(new Lt(space.nextConstraintId, null, y, x, z))
        for (a <- x.domain.values; b <- y.domain.values) {
            space.setValue(x, a).setValue(y, b).initialize
            assertEq(now.value(z).truthValue, a > b)
            val TighteningResult(tightenedState, maybeTightenedVariable) = objective.tighten(space)
            assertEq(tightenedState.mappedVariables, Set(x, y, z))
            if (a > b) {
                assertEq(tightenedState.value(x), a)
                assertEq(tightenedState.value(y), a - One)
                assertEq(maybeTightenedVariable, Some(y))
                assertEq(x.domain, baseDomain)
                assertEq(y.domain.lb, a)
                assertEq(now.value(x), a)
                assertEq(now.value(y), a)
            } else {
                assertEq(tightenedState.value(x), a)
                assertEq(tightenedState.value(y), b)
                assertEq(maybeTightenedVariable, None)
                assertEq(x.domain, baseDomain)
                assertEq(y.domain, baseDomain)
                assertEq(now.value(x), a)
                assertEq(now.value(y), b)
            }
            assertEq(now.value(z).truthValue, false)
            x.relaxDomain(baseDomain)
            y.relaxDomain(baseDomain)
        }
    }

}
