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
final class AssignmentPropagationTestToolingTest extends UnitTest with AssignmentPropagationTestTooling {

    private val space = new Space(logger, sigint)
    private val now = space.searchState
    private val x = new IntegerVariable(space.nextVariableId, "x", CompleteIntegerRange)
    private val y = new IntegerVariable(space.nextVariableId, "y", CompleteIntegerRange)
    private val costs = new BooleanVariable(space.nextVariableId, "costs", CompleteBooleanDomain)
    space.post(new Eq(space.nextConstraintId, None, x, y, costs))

    @Test
    def testInitialize: Unit = {
        runScenario(TestScenario(space, Initialize("initialize", (x, Zero), (y, One), (costs, False))))
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
        space.initialize()
        runScenario(TestScenario(space, Consult("consult", (x, One), (y, Two), (costs, False))))
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
        space.initialize()
        runScenario(TestScenario(space, ConsultAndCommit("consult & commit", (x, One), (y, Two), (costs, False))))
        assertEq(space.numberOfInitializations, 1)
        assertEq(space.numberOfConsultations, 1)
        assertEq(space.numberOfCommitments, 1)
        assertEq(now.value(x), One)
        assertEq(now.value(y), Two)
        assertEq(now.value(costs), False)
    }

}
