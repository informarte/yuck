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
final class SumTest extends UnitTest {

    @Test
    def testSum {
        val space = new Space(logger)
        val d = new IntegerRange(Zero, new IntegerValue(100))
        val x1 = new IntegerVariable(space.variableIdFactory.nextId, "x1", d)
        val x2 = new IntegerVariable(space.variableIdFactory.nextId, "x2", d)
        val x3 = new IntegerVariable(space.variableIdFactory.nextId, "x3", d)
        val y = new IntegerVariable(space.variableIdFactory.nextId, "y", d)
        val c = new Sum(space.constraintIdFactory.nextId, null, List(x1, x2, x3), y)
        space
            .post(c)
            .setValue(x1, One)
            .setValue(x2, Two)
            .setValue(x3, Three)
            .initialize
        assertEq(space.searchVariables, Set(x1, x2, x3))
        val now = space.searchState
        assertEq(now.value(y), Six)
        val move = new ChangeValue(space.moveIdFactory.nextId, x2, Three)
        val after = space.consult(move)
        assertEq(after.value(y), Seven)
        space.commit(move)
        assertEq(now.value(y), Seven)
        space.initialize
        assertEq(now.value(y), Seven)
    }

}
