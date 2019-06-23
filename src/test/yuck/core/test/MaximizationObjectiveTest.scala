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
    private val baseDomain = new IntegerRange(Zero, Nine)
    private val x = new IntegerVariable(space.nextVariableId, "x", baseDomain)
    private val objective = new MaximizationObjective(x, Zero, Some(One))

    @Test
    def testCostComparison {
        val a = new Assignment
        a.setValue(x, Zero)
        assertEq(objective.costs(a), Zero)
        assertEq(objective.compareCosts(Zero, Zero), 0)
        assertGt(objective.compareCosts(Zero, One), 0)
        assertLt(objective.compareCosts(One, Zero), 0)
        assertEq(objective.assessMove(a, a), 0)
    }

    @Test
    def testMoveAssessment {
        val a = new Assignment
        a.setValue(x, Zero)
        val b = new Assignment
        b.setValue(x, One)
        assertEq(objective.assessMove(b, b), 0)
        assertEq(objective.assessMove(a, b), -1)
        assertEq(objective.assessMove(a, b), -1)
        b.setValue(x, Two)
        assertLt(objective.assessMove(a, b), -1)
        objective.assessMove(b, a)
        assertGt(objective.assessMove(b, a), 1)
    }

    @Test
    def testTightening {
        space
            .post(new DummyConstraint(space.nextConstraintId, List(x), Nil))
            .setValue(x, Eight)
            .initialize
        // check that tightening finds upper bound of x
        if (true) {
            val tighteningResult = objective.tighten(space)
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
            val tighteningResult = objective.tighten(space)
            assert(tighteningResult._1.eq(space.searchState))
            assertEq(tighteningResult._2, None)
        }
    }

}
