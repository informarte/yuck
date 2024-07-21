package yuck.core.test

import org.junit.*

import yuck.core.*
import yuck.test.*
import yuck.test.util.UnitTest

/**
  * @author Michael Marte
  *
  */
@FixMethodOrder(runners.MethodSorters.NAME_ASCENDING)
final class MinimizationObjectiveTest extends UnitTest {

    private val BaseDomain = IntegerRange(0, 9)

    private val space = new Space(logger, sigint)
    private val now = space.searchState

    private val x = new IntegerVariable(space.nextVariableId(), "x", BaseDomain)
    private val y = new IntegerVariable(space.nextVariableId(), "y", BaseDomain)
    private val objective = new MinimizationObjective(x, Some(BaseDomain.lb + One), Some(y))

    @Test
    def testBasics(): Unit = {
        assertEq(objective.optimizationMode, OptimizationMode.Min)
        assertEq(objective.targetCosts, BaseDomain.lb + One)
        assertEq(objective.primitiveObjectives, Seq(objective))
        assertEq(objective.objectiveVariables, Seq(x))
        for (a <- x.domain.values) {
            space.setValue(x, a)
            assertEq(objective.costs(now), a)
            val isSolution = a <= BaseDomain.lb + One
            val isOptimal = a == BaseDomain.lb
            assertEq(objective.isSolution(a), isSolution)
            assertEq(objective.isSolution(now), isSolution)
            assertEq(objective.isGoodEnough(a), isSolution)
            assertEq(objective.isGoodEnough(now), isSolution)
            assertEq(objective.isOptimal(a), isOptimal)
            assertEq(objective.isOptimal(now), isOptimal)
        }
    }

    @Test
    def testCostComparison(): Unit = {
        assertEq(objective.compareCosts(Zero, Zero), 0)
        assertLt(objective.compareCosts(Zero, One), 0)
        assertGt(objective.compareCosts(One, Zero), 0)
    }

    @Test
    def testMoveAssessment(): Unit = {
        import scala.math.Ordering.Double.TotalOrdering
        val a = new HashMapBackedAssignment
        a.setValue(x, Zero)
        val b = new HashMapBackedAssignment
        b.setValue(x, One)
        assertEq(objective.assessMove(a, a), 0.0)
        assertEq(objective.assessMove(b, b), 0.0)
        assertEq(objective.assessMove(a, b), 1.0)
        b.setValue(x, Two)
        assertGt(objective.assessMove(a, b), 1.0)
        b.setValue(x, Three)
        assertGt(objective.assessMove(a, b), 1.0)
        assertLt(objective.assessMove(b, a), -1.0)
    }

    @Test
    def testSearchForActualObjectiveValue(): Unit = {
        space
            .post(new DummyConstraint(space.nextConstraintId(), List(x), Nil))
            .registerObjectiveVariable(x)
            .initialize()
        for (a <- x.domain.values) {
            space.setValue(x, a)
            objective.findActualObjectiveValue(space)
            assertEq(now.value(x), x.domain.lb)
            assertEq(x.domain, BaseDomain)
            assertEq(y.domain, BaseDomain)
        }
    }

    @Test
    def testTightening(): Unit = {
        space
            .post(new DummyConstraint(space.nextConstraintId(), List(x), Nil))
            .setValue(y, y.domain.ub)
            .initialize()
        for (a <- x.domain.values) {
            space.setValue(x, a)
            val tightenedVariables = objective.tighten(space)
            assertEq(now.value(x), a)
            assertEq(now.value(y), a)
            assertEq(x.domain, BaseDomain)
            assertEq(y.domain, IntegerRange(BaseDomain.lb, a))
            assertEq(tightenedVariables.isEmpty, y.domain == BaseDomain)
            y.relaxDomain(BaseDomain)
        }
    }

}
