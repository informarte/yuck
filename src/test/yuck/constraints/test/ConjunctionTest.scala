package yuck.constraints.test

import org.junit._

import scala.collection._

import yuck.constraints._
import yuck.constraints.test.util.ConstraintTestTooling
import yuck.core._
import yuck.test.util.UnitTest

/**
 * @author Michael Marte
 *
 */
@FixMethodOrder(runners.MethodSorters.NAME_ASCENDING)
final class ConjunctionTest extends UnitTest with ConstraintTestTooling {

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
            TestScenario(
                space,
                Initialize("setup", (x1, False), (x2, False2), (x3, False3), (y, False6)),
                ConsultAndCommit("1", (x2, False3), (y, False7)),
                ConsultAndCommit("2", (x1, True), (x3, False2), (y, False5))))
    }

}
