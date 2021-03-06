package yuck.constraints.test

import org.junit._

import scala.collection._

import yuck.constraints._
import yuck.core._
import yuck.util.testing.UnitTest

/**
 * @author Michael Marte
 *
 */
@Test
@FixMethodOrder(runners.MethodSorters.NAME_ASCENDING)
final class ConjunctionTest extends UnitTest with CostComputationTestTooling[BooleanValue] {

    @Test
    def testConjunction: Unit = {
        val space = new Space(logger, sigint)
        val d = CompleteBooleanDecisionDomain
        val x1 = new BooleanVariable(space.nextVariableId, "x1", d)
        val x2 = new BooleanVariable(space.nextVariableId, "x2", d)
        val x3 = new BooleanVariable(space.nextVariableId, "x3", d)
        val y = new BooleanVariable(space.nextVariableId, "y", d)
        space.post(new Conjunction(space.nextConstraintId, null, List(x1, x2, x3), y))
        assertEq(space.searchVariables, Set(x1, x2, x3))
        runScenario(
            CostComputationTestScenario(
                space,
                y,
                Initialize("setup", False6, (x1, False), (x2, False2), (x3, False3)),
                ConsultAndCommit("1", False7, (x2, False3)),
                ConsultAndCommit("2", False5, (x1, True), (x3, False2))))
    }

}
