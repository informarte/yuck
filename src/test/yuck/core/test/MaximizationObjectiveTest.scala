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
final class MaximizationObjectiveTest extends UnitTest {

    private val space = new Space(logger, sigint)
    private val now = space.searchState
    private val baseDomain = IntegerRange(Zero, Nine)
    private val x = new IntegerVariable(space.nextVariableId, "x", baseDomain)
    private val y = new IntegerVariable(space.nextVariableId, "y", baseDomain)
    private val objective = new MaximizationObjective(x, Some(baseDomain.ub - One), Some(y))

    @Test
    def testBasics: Unit = {
        assertEq(objective.optimizationMode, OptimizationMode.Max)
        assertEq(objective.targetCosts, baseDomain.ub - One)
        assertEq(objective.primitiveObjectives, Seq(objective))
        assertEq(objective.objectiveVariables, Seq(x))
        for (a <- x.domain.values) {
            space.setValue(x, a)
            assertEq(objective.costs(now), a)
            val isSolution = a >= baseDomain.ub - One
            val isOptimal = a == baseDomain.ub
            assertEq(objective.isSolution(a), isSolution)
            assertEq(objective.isSolution(now), isSolution)
            assertEq(objective.isGoodEnough(a), isSolution)
            assertEq(objective.isGoodEnough(now), isSolution)
            assertEq(objective.isOptimal(a), isOptimal)
            assertEq(objective.isOptimal(now), isOptimal)
        }
    }

    @Test
    def testCostComparison: Unit = {
        assertEq(objective.compareCosts(Zero, Zero), 0)
        assertGt(objective.compareCosts(Zero, One), 0)
        assertLt(objective.compareCosts(One, Zero), 0)
    }

    @Test
    def testMoveAssessment: Unit = {
        import scala.math.Ordering.Double.TotalOrdering
        val a = new HashMapBackedAssignment
        a.setValue(x, Zero)
        val b = new HashMapBackedAssignment
        b.setValue(x, One)
        assertEq(objective.assessMove(a, a), 0)
        assertEq(objective.assessMove(b, b), 0)
        assertEq(objective.assessMove(a, b), -1)
        b.setValue(x, Two)
        assertLt(objective.assessMove(a, b), -1)
        assertGt(objective.assessMove(b, a), 1)
    }

    @Test
    def testSearchForActualObjectiveValue: Unit = {
        for (a <- x.domain.values) {
            space
                .post(new DummyConstraint(space.nextConstraintId, List(x), Nil))
                .setValue(x, a)
                .initialize()
            objective.findActualObjectiveValue(space)
            assertEq(now.value(x), x.domain.ub)
            assertEq(x.domain, baseDomain)
            assertEq(y.domain, baseDomain)
        }
    }

    @Test
    def testTightening: Unit = {
        for (a <- x.domain.values) {
            space
                .post(new DummyConstraint(space.nextConstraintId, List(x), Nil))
                .setValue(x, a)
                .setValue(y, y.domain.lb)
                .initialize()
            val tightenedVariables = objective.tighten(space)
            assertEq(now.value(x), a)
            assertEq(now.value(y), a)
            assertEq(x.domain, baseDomain)
            assertEq(y.domain, IntegerRange(a, baseDomain.ub))
            assertEq(tightenedVariables.isEmpty, y.domain == baseDomain)
            y.relaxDomain(baseDomain)
        }
    }

}
