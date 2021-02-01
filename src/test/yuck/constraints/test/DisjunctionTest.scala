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
final class DisjunctionTest extends UnitTest with AssignmentPropagationTestTooling {

    @Test
    def testDisjunction: Unit = {
        val space = new Space(logger, sigint)
        val d = CompleteBooleanDomain
        val s = new BooleanVariable(space.nextVariableId, "s", d)
        val t = new BooleanVariable(space.nextVariableId, "t", d)
        val u = new BooleanVariable(space.nextVariableId, "u", d)
        val costs = new BooleanVariable(space.nextVariableId, "costs", CompleteBooleanDomain)
        space.post(new Disjunction(space.nextConstraintId, null, List(s, t, u), costs))
        assertEq(space.searchVariables, Set(s, t, u))
        runScenario(
            TestScenario(
                space,
                Initialize("init", (s, False), (t, False2), (u, False3), (costs, False2)),
                ConsultAndCommit("1", (u, False2), (costs, False)),
                ConsultAndCommit("2", (s, True), (costs, True)),
                ConsultAndCommit("2", (s, False3), (costs, False2))))
    }

}
