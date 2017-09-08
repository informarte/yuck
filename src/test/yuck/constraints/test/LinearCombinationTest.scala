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
    def testLinearCombination {
        val space = new Space(logger)
        val d = new IntegerDomain(Zero, new IntegerValue(100))
        val x1 = space.createVariable("x1", d)
        val x2 = space.createVariable("x2", d)
        val x3 = space.createVariable("x3", d)
        val y = space.createVariable("y", d)
        val c =
            new LinearCombination(
                space.constraintIdFactory.nextId, null,
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
        val move = new ChangeValue(space.moveIdFactory.nextId, x2, Three)
        val after = space.consult(move)
        assertEq(now.value(y), Five)
        assertEq(after.value(y), Six)
        space.commit(move)
        assertEq(now.value(y), Six)
        space.initialize
        assertEq(now.value(y), Six)
    }

}
