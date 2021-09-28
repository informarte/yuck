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
final class SumTest extends UnitTest with ConstraintTestTooling {

    @Test
    def testSum: Unit = {
        val space = new Space(logger, sigint)
        val d = IntegerRange(Zero, IntegerValue(100))
        val x1 = new IntegerVariable(space.nextVariableId, "x1", d)
        val x2 = new IntegerVariable(space.nextVariableId, "x2", d)
        val x3 = new IntegerVariable(space.nextVariableId, "x3", d)
        val y = new IntegerVariable(space.nextVariableId, "y", d)
        space.post(new Sum(space.nextConstraintId, null, List(x1, x2, x3), y))
        assertEq(space.searchVariables, Set(x1, x2, x3))
        runScenario(
            TestScenario(
                space,
                Initialize("setup", (x1, One), (x2, Two), (x3, Three), (y, Six)),
                ConsultAndCommit("1", (x2, Three), (y, Seven)),
                ConsultAndCommit("2", (x1, Zero), (x3, Two), (y, Five))))
    }

}
