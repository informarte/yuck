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
final class MaximumTest extends UnitTest with ConstraintTestTooling {

    @Test
    def testMaximum: Unit = {
        val space = new Space(logger, sigint)
        val d = IntegerRange(Zero, Nine)
        val s = new IntegerVariable(space.nextVariableId, "s", d)
        val t = new IntegerVariable(space.nextVariableId, "t", d)
        val u = new IntegerVariable(space.nextVariableId, "u", d)
        val max = new IntegerVariable(space.nextVariableId, "costs", CompleteIntegerRange)
        space.post(new Maximum(space.nextConstraintId, null, List(s, t, u), max))
        assertEq(space.searchVariables, Set(s, t, u))
        runScenario(
            TestScenario(
                space,
                Initialize("setup", (s, One), (t, Two), (u, Three), (max, Three)),
                ConsultAndCommit("1", (u, Two), (max, Two)),
                ConsultAndCommit("2", (s, Three), (max, Three))))
    }

}
