package yuck.core.test

import org.junit.*

import yuck.constraints.{Le, Lt}
import yuck.core.*
import yuck.test.*
import yuck.test.util.UnitTest

/**
  * @author Michael Marte
  *
  */
@FixMethodOrder(runners.MethodSorters.NAME_ASCENDING)
final class HierarchicalObjectiveTest extends UnitTest {

    private val BaseDomain = IntegerRange(0, 9)

    private val space = new Space(logger, sigint)
    private val now = space.searchState

    private val x = new IntegerVariable(space.nextVariableId(), "x", BaseDomain)
    private val y = new IntegerVariable(space.nextVariableId(), "y", BaseDomain)
    private val z = new IntegerVariable(space.nextVariableId(), "z", BaseDomain)

    @Test
    def testBasics(): Unit = {
        val mainObjective = new MinimizationObjective(x, Some(BaseDomain.lb), None)
        val subordinateObjective = new MaximizationObjective(y, Some(BaseDomain.ub - One), None)
        val objective = new HierarchicalObjective(List(mainObjective, subordinateObjective), false, false)
        assertEq(objective.targetCosts, new PolymorphicListValue(List(BaseDomain.lb, BaseDomain.ub - One)))
        assertEq(objective.primitiveObjectives, Seq(mainObjective, subordinateObjective))
        assertEq(objective.objectiveVariables, Seq(x, y))
        for (a <- x.domain.values; b <- y.domain.values) {
            space.setValue(x, a).setValue(y, b)
            val ab = new PolymorphicListValue(List(a, b))
            assertEq(objective.costs(now), ab)
            val isSolution = a == BaseDomain.lb
            val isGoodEnough = isSolution && b >= BaseDomain.ub - One
            val isOptimal = isSolution && b == BaseDomain.ub
            assertEq(objective.isSolution(ab), isSolution)
            assertEq(objective.isSolution(now), isSolution)
            assertEq(objective.isGoodEnough(ab), isGoodEnough)
            assertEq(objective.isGoodEnough(now), isGoodEnough)
            assertEq(objective.isOptimal(ab), isOptimal)
            assertEq(objective.isOptimal(now), isOptimal)
        }
    }

    @Test
    def testCostComparison(): Unit = {
        val mainObjective = new MinimizationObjective(x, Some(Zero), None)
        val subordinateObjective = new MaximizationObjective(y, Some(Nine), None)
        val objective = new HierarchicalObjective(List(mainObjective, subordinateObjective), false, false)
        val a = new HashMapBackedAssignment
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
    def testMoveAssessment(): Unit = {
        import scala.math.Ordering.Double.TotalOrdering
        val mainObjective = new MinimizationObjective(x, Some(Zero), None)
        val subordinateObjective = new MaximizationObjective(y, Some(Nine), None)
        val objective = new HierarchicalObjective(List(mainObjective, subordinateObjective), false, false)
        val a = new HashMapBackedAssignment
        a.setValue(x, Zero)
        a.setValue(y, One)
        val b = new HashMapBackedAssignment
        b.setValue(x, One)
        b.setValue(y, One)
        // check move assessment wrt. main (minimization) goal
        assertEq(objective.assessMove(b, b), 0.0)
        assertEq(objective.assessMove(a, b), 1.0)
        assertEq(objective.assessMove(a, b), 1.0)
        b.setValue(x, Two)
        assertGt(objective.assessMove(a, b), 1.0)
        objective.assessMove(b, a)
        assertLt(objective.assessMove(b, a), -1.0)
        // check move assessment wrt. subordinate (maximization) goal
        b.setValue(x, Zero)
        a.setValue(y, Zero)
        b.setValue(y, One)
        assertEq(objective.assessMove(b, b), 0.0)
        assertEq(objective.assessMove(a, b), -1.0)
        assertEq(objective.assessMove(a, b), -1.0)
        b.setValue(y, Two)
        assertLt(objective.assessMove(a, b), -1.0)
        objective.assessMove(b, a)
        assertGt(objective.assessMove(b, a), 1.0)
    }

    @Test
    def testSearchForActualObjectiveValueWhenMinimizing(): Unit = {
        val costs = new BooleanVariable(space.nextVariableId(), "costs", TrueDomain)
        val mainObjective = new SatisfactionObjective(costs)
        val subordinateObjective = new MinimizationObjective(y, None, None)
        val objective = new HierarchicalObjective(List(mainObjective, subordinateObjective), false, false)
        space
            .post(new Le(space.nextConstraintId(), null, x, y, costs))
            .registerObjectiveVariable(y)
            .registerObjectiveVariable(costs)
        for (a <- x.domain.values) {
            space.setValue(x, a).setValue(y, y.domain.ub).initialize()
            objective.findActualObjectiveValue(space)
            assertEq(now.value(y), a)
            assert(now.value(costs).truthValue)
            assertEq(x.domain, BaseDomain)
            assertEq(y.domain, BaseDomain)
        }
    }

    @Test
    def testTighteningWhenMinimizing(): Unit = {
        val costs = new BooleanVariable(space.nextVariableId(), "costs", TrueDomain)
        val mainObjective = new SatisfactionObjective(costs)
        val subordinateObjective = new MinimizationObjective(y, None, Some(z))
        val objective = new HierarchicalObjective(List(mainObjective, subordinateObjective), false, false)
        space
            .post(new Le(space.nextConstraintId(), null, x, y, costs))
            .setValue(z, z.domain.ub)
        for (a <- x.domain.values; b <- y.domain.values) {
            space.setValue(x, a).setValue(y, b).initialize()
            val tightenedVariables = objective.tighten(space)
            assertEq(now.value(x), a)
            assertEq(x.domain, BaseDomain)
            assertEq(now.value(y), b)
            assertEq(y.domain, BaseDomain)
            if (objective.isSolution(now) && b < y.domain.ub) {
                assertEq(tightenedVariables, Set(z))
                assertEq(now.value(z), b)
                assertEq(z.domain, IntegerRange(BaseDomain.lb, b))
                z.relaxDomain(BaseDomain)
                space.setValue(z, z.domain.ub)
            } else {
                assert(tightenedVariables.isEmpty)
                assertEq(now.value(z), z.domain.ub)
                assertEq(z.domain, BaseDomain)
            }
        }
    }

    @Test
    def testSearchForActualObjectiveValueWhenMaximizing(): Unit = {
        val costs = new BooleanVariable(space.nextVariableId(), "costs", TrueDomain)
        val mainObjective = new SatisfactionObjective(costs)
        space.registerObjectiveVariable(costs).registerObjectiveVariable(y)
        val subordinateObjective = new MaximizationObjective(y, None, None)
        val objective = new HierarchicalObjective(List(mainObjective, subordinateObjective), false, false)
        space
            .post(new Le(space.nextConstraintId(), null, y, x, costs))
            .registerObjectiveVariable(costs)
            .registerObjectiveVariable(y)
        for (a <- x.domain.values) {
            space.setValue(x, a).setValue(y, y.domain.lb).initialize()
            objective.findActualObjectiveValue(space)
            assertEq(now.value(y), a)
            assert(now.value(costs).truthValue)
            assertEq(x.domain, BaseDomain)
            assertEq(y.domain, BaseDomain)
        }
    }

    @Test
    def testTighteningWhenMaximizing(): Unit = {
        val costs = new BooleanVariable(space.nextVariableId(), "costs", TrueDomain)
        val mainObjective = new SatisfactionObjective(costs)
        val subordinateObjective = new MaximizationObjective(y, None, Some(z))
        val objective = new HierarchicalObjective(List(mainObjective, subordinateObjective), false, false)
        space
            .post(new Le(space.nextConstraintId(), null, y, x, costs))
            .setValue(z, z.domain.lb)
        for (a <- x.domain.values; b <- y.domain.values) {
            space.setValue(x, a).setValue(y, b).initialize()
            val tightenedVariables = objective.tighten(space)
            assertEq(now.value(x), a)
            assertEq(x.domain, BaseDomain)
            assertEq(now.value(y), b)
            assertEq(y.domain, BaseDomain)
            if (objective.isSolution(now) && b > y.domain.lb) {
                assertEq(tightenedVariables, Set(z))
                assertEq(now.value(z), b)
                assertEq(z.domain, IntegerRange(b, BaseDomain.ub))
                z.relaxDomain(BaseDomain)
                space.setValue(z, z.domain.lb)
            } else {
                assert(tightenedVariables.isEmpty)
                assertEq(now.value(z), z.domain.lb)
                assertEq(z.domain, BaseDomain)
            }
        }
    }

}
