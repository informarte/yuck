package yuck.core.test

import org.junit._

import yuck.core._
import yuck.util.testing.UnitTest

/**
 * @author Michael Marte
 *
 */
@FixMethodOrder(org.junit.runners.MethodSorters.NAME_ASCENDING)
final class IntegerRangeTest extends UnitTest {

    private def range(a: Int, b: Int) = new IntegerRange(IntegerValue.get(a), IntegerValue.get(b))

    @Test
    def testMultiplication {
        assertEq(range(0, 2).mult(range(1, 2)), range(0, 4))
        assertEq(range(0, 2).mult(range(-1, 2)), range(-2, 4))
        assertEq(range(-2, 2).mult(range(3, 10)), range(-20, 20))
    }

    @Test
    def testDivision {
        assertEq(range(-1, 100).div(range(-2, 8)), UnboundedIntegerRange)
        assertEq(range(10, 100).div(range(0, 0)), EmptyIntegerRange)
        assertEq(range(-100, -10).div(range(-2, 5)), range(-100, 100))
        assertEq(range(1, 100).div(range(-7, 0)), range(1, 100).div(range(-7, -1)))
        assertEq(range(1, 100).div(range(0, 7)), range(1, 100).div(range(1, 7)))
        assertEq(range(155, 161).div(range(9, 11)), range(15, 17))
    }

}
