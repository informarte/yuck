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
final class MinimizationObjectiveTest extends UnitTest {

    private val space = new Space(logger, sigint)
    private val baseDomain = new IntegerRange(Zero, Nine)
    private val x = new IntegerVariable(space.nextVariableId, "x", baseDomain)
    private val objective = new MinimizationObjective(x, Zero, Some(MinusOne))

    @Test
    def testCostComparison: Unit = {
        val a = new Assignment
        a.setValue(x, Zero)
        assertEq(objective.costs(a), Zero)
        assertEq(objective.compareCosts(Zero, Zero), 0)
        assertLt(objective.compareCosts(Zero, One), 0)
        assertGt(objective.compareCosts(One, Zero), 0)
        assertEq(objective.assessMove(a, a), 0)
    }

    @Test
    def testMoveAssessment: Unit = {
        import scala.math.Ordering.Double.TotalOrdering
        val a = new Assignment
        a.setValue(x, Zero)
        val b = new Assignment
        b.setValue(x, One)
        assertEq(objective.assessMove(b, b), 0)
        assertEq(objective.assessMove(a, b), 1)
        assertEq(objective.assessMove(a, b), 1)
        b.setValue(x, Two)
        assertGt(objective.assessMove(a, b), 1)
        b.setValue(x, Three)
        assertGt(objective.assessMove(a, b), 1)
        objective.assessMove(b, a)
        assertLt(objective.assessMove(b, a), -1)
    }

    @Test
    def testTightening: Unit = {
        space
            .post(new DummyConstraint(space.nextConstraintId, List(x), Nil))
            .setValue(x, One)
            .initialize
        // check that tightening finds lower bound of x
        if (true) {
            val TighteningResult(tightenedState, maybeTightenedVariable) = objective.tighten(space)
            assertEq(tightenedState.mappedVariables, Set(x))
            assertEq(tightenedState.value(x), Zero)
            assertEq(maybeTightenedVariable, Some(x))
            assertEq(space.searchState.value(x), Zero)
            assert(x.domain.isSingleton)
            assertEq(x.domain.singleValue, Zero)
        }
        // check that further tightening is not possible
        if (true) {
            val TighteningResult(tightenedState, maybeTightenedObjective) = objective.tighten(space)
            assertEq(maybeTightenedObjective, None)
        }
    }

}
