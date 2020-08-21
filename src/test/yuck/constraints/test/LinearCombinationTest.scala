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
final class LinearCombinationTest extends UnitTest with CostComputationTestTooling[IntegerValue] {

    @Test
    def testLinearCombination: Unit = {
        val space = new Space(logger, sigint)
        val d = new IntegerRange(Zero, new IntegerValue(100))
        val x1 = new IntegerVariable(space.nextVariableId, "x1", d)
        val x2 = new IntegerVariable(space.nextVariableId, "x2", d)
        val x3 = new IntegerVariable(space.nextVariableId, "x3", d)
        val y = new IntegerVariable(space.nextVariableId, "y", d)
        space.post(
            new LinearCombination(
                space.nextConstraintId, null,
                List(new AX(Zero, x1), new AX(One, x2), new AX(One, x3)), y))
        assertEq(space.searchVariables, Set(x1, x2, x3))
        runScenario(
            CostComputationTestScenario(
                space,
                y,
                Initialize("setup", Five, (x1, One), (x2, Two), (x3, Three)),
                ConsultAndCommit("1", Six, (x2, Three))))
    }

}
