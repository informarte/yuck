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
final class LinearCombinationTest extends UnitTest {

    @Test
    def testLinearCombination: Unit = {
        val space = new Space(logger)
        val d = new IntegerRange(Zero, new IntegerValue(100))
        val x1 = new IntegerVariable(space.nextVariableId, "x1", d)
        val x2 = new IntegerVariable(space.nextVariableId, "x2", d)
        val x3 = new IntegerVariable(space.nextVariableId, "x3", d)
        val y = new IntegerVariable(space.nextVariableId, "y", d)
        val c =
            new LinearCombination(
                space.nextConstraintId, null,
                List(new AX(Zero, x1), new AX(One, x2), new AX(One, x3)), y)
        space
            .post(c)
            .setValue(x1, One)
            .setValue(x2, Two)
            .setValue(x3, Three)
            .initialize
        assertEq(space.searchVariables, Set(x1, x2, x3))
        val now = space.searchState
        assertEq(now.value(y), Five)
        val move = new ChangeValue(space.nextMoveId, x2, Three)
        val after = space.consult(move)
        assertEq(after.value(y), Six)
        space.commit(move)
        assertEq(now.value(y), Six)
        space.initialize
        assertEq(now.value(y), Six)
    }

}
