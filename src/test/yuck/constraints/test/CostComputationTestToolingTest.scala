package yuck.constraints.test

import org.junit._

import yuck.constraints.Eq
import yuck.core._
import yuck.util.testing.UnitTest

/**
 * @author Michael Marte
 *
 */
@Test
@FixMethodOrder(runners.MethodSorters.NAME_ASCENDING)
final class CostComputationTestToolingTest extends UnitTest with CostComputationTestTooling[BooleanValue] {

    private val space = new Space(logger, sigint)
    private val now = space.searchState
    private val x = new IntegerVariable(space.nextVariableId, "x", CompleteIntegerRange)
    private val y = new IntegerVariable(space.nextVariableId, "y", CompleteIntegerRange)
    private val costs = new BooleanVariable(space.nextVariableId, "costs", CompleteBooleanDomain)
    space.post(new Eq(space.nextConstraintId, None, x, y, costs))

    @Test
    def testInitialize: Unit = {
        runScenario(CostComputationTestScenario(space, costs, Initialize("initialize", False, (x, Zero), (y, One))))
        assertEq(space.numberOfInitializations, 1)
        assertEq(space.numberOfConsultations, 0)
        assertEq(space.numberOfCommitments, 0)
        assertEq(now.value(x), Zero)
        assertEq(now.value(y), One)
        assertEq(now.value(costs), False)
    }

    @Test
    def testConsult: Unit = {
        space.setValue(x, Zero).setValue(y, Zero)
        space.initialize
        runScenario(CostComputationTestScenario(space, costs, Consult("consult", False, (x, One), (y, Two))))
        assertEq(space.numberOfInitializations, 1)
        assertEq(space.numberOfConsultations, 1)
        assertEq(space.numberOfCommitments, 0)
        assertEq(now.value(x), Zero)
        assertEq(now.value(y), Zero)
        assertEq(now.value(costs), True)
    }

    @Test
    def testConsultAndCommit: Unit = {
        space.setValue(x, Zero).setValue(y, Zero)
        space.initialize
        runScenario(CostComputationTestScenario(space, costs, ConsultAndCommit("consult & commit", False, (x, One), (y, Two))))
        assertEq(space.numberOfInitializations, 1)
        assertEq(space.numberOfConsultations, 1)
        assertEq(space.numberOfCommitments, 1)
        assertEq(now.value(x), One)
        assertEq(now.value(y), Two)
        assertEq(now.value(costs), False)
    }

}
