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
final class MinimumTest extends UnitTest with ConstraintTestTooling {

    @Test
    def testMinimum: Unit = {
        val space = new Space(logger, sigint)
        val d = IntegerRange(Zero, Nine)
        val s = new IntegerVariable(space.nextVariableId, "s", d)
        val t = new IntegerVariable(space.nextVariableId, "t", d)
        val u = new IntegerVariable(space.nextVariableId, "u", d)
        val min = new IntegerVariable(space.nextVariableId, "costs", CompleteIntegerRange)
        space.post(new Minimum(space.nextConstraintId, null, List(s, t, u), min))
        assertEq(space.searchVariables, Set(s, t, u))
        runScenario(
            TestScenario(
                space,
                Initialize("setup", (s, One), (t, Two), (u, Three), (min, One)),
                ConsultAndCommit("1", (s, Two), (min, Two)),
                ConsultAndCommit("2", (u, One), (min, One))))
    }

}
