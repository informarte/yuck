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
        val x1 = space.createVariable("x1", d)
        val x2 = space.createVariable("x2", d)
        val x3 = space.createVariable("x3", d)
        val y = space.createVariable("y", d)
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
        assertEq(now.value(y), Six)
        assertEq(after.value(y), Seven)
        space.commit(move)
        assertEq(now.value(y), Seven)
        space.initialize
        assertEq(now.value(y), Seven)
    }

}
