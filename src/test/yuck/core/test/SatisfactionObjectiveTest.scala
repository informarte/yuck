package yuck.core.test

import org.junit.*

import yuck.core.{given, *}
import yuck.test.util.UnitTest

/**
 * @author Michael Marte
 *
 */
@FixMethodOrder(runners.MethodSorters.NAME_ASCENDING)
final class SatisfactionObjectiveTest extends UnitTest {

    private val space = new Space(logger, sigint)
    private val now = space.searchState
    private val x = new BooleanVariable(space.nextVariableId(), "x", CompleteBooleanDomain)
    private val objective = new SatisfactionObjective(x)

    @Test
    def testBasics(): Unit = {
        assertEq(objective.optimizationMode, OptimizationMode.Min)
        assertEq(objective.targetCosts, True)
        assertEq(objective.primitiveObjectives, Seq(objective))
        assertEq(objective.objectiveVariables, Seq(x))
        for (a <- x.domain.values) {
            space.setValue(x, a)
            assertEq(objective.costs(now), a)
            val isSolution = a == True
            assertEq(objective.isSolution(a), isSolution)
            assertEq(objective.isSolution(now), isSolution)
            assertEq(objective.isGoodEnough(a), isSolution)
            assertEq(objective.isGoodEnough(now), isSolution)
            assertEq(objective.isOptimal(a), isSolution)
            assertEq(objective.isOptimal(now), isSolution)
        }
    }

    @Test
    def testCostComparison(): Unit = {
        assertEq(objective.compareCosts(True, True), 0)
        assertGt(objective.compareCosts(False, True), 0)
        assertLt(objective.compareCosts(True, False), 0)
        assertEq(objective.compareCosts(False, False), 0)
    }

    @Test
    def testMoveAssessment(): Unit = {
        import scala.math.Ordering.Double.TotalOrdering
        val a = new HashMapBackedAssignment
        a.setValue(x, True)
        val b = new HashMapBackedAssignment
        b.setValue(x, False)
        assertEq(objective.assessMove(a, a), 0.0)
        assertEq(objective.assessMove(b, b), 0.0)
        assertEq(objective.assessMove(a, b), 1.0)
        b.setValue(x, False2)
        assertGt(objective.assessMove(a, b), 1.0)
        assertLt(objective.assessMove(b, a), 1.0)
    }

}
