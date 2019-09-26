package yuck.core.test

import org.junit._

import yuck.core._
import yuck.util.testing.{OrderingTestHelper, UnitTest}

/**
 * @author Michael Marte
 *
 */
@Test
@FixMethodOrder(runners.MethodSorters.NAME_ASCENDING)
final class BooleanValueOperationsTest extends UnitTest with BooleanValueTestData {

    private val num = BooleanValueOperations
    private val randomGenerator = new JavaRandomGenerator

    @Test
    def testValueFactory: Unit = {
        for (a <- testRange) {
            assertEq(num.fromInt(a).violation, a)
        }
    }

    @Test
    def testSpecialValues: Unit = {
        assertEq(num.zero, True)
        assertEq(num.one, False)
    }

    @Test
    def testOrdering: Unit = {
        val helper = new OrderingTestHelper[BooleanValue](randomGenerator)
        helper.testOrdering(testData, num)
        val ord = new Ordering[BooleanValue] {
            override def compare(a: BooleanValue, b: BooleanValue) = a.compare(b)
        }
        assertEq(testData.sorted(num), testData.sorted(ord))
    }

    @Test
    def testNumericalOperations: Unit = {
        for (a <- testData) {
            for (b <- testData) {
                assertEq(num.plus(a, b), a + b)
                if (b.violation > a.violation) {
                    assertEx(num.minus(a, b))
                } else {
                    assertEq(num.minus(a, b).violation, a.violation - b.violation)
                }
                assertEq(num.times(a, b), a * b)
            }
            assertEq(num.abs(a), a.abs)
            assertEx(num.negate(a))
            assertEq(num.toInt(a), a.toInt)
            assertEq(num.toLong(a), a.toLong)
            assertEq(num.toFloat(a), a.toFloat)
            assertEq(num.toDouble(a), a.toDouble)
        }
    }

    @Test
    def testOverflowCheckingInNumericalOperations: Unit = {
        num.plus(BooleanValue.get(Long.MaxValue), True)
        assertEx(num.plus(BooleanValue.get(Long.MaxValue), False), classOf[ArithmeticException])
        num.minus(True, True)
        assertEx(num.minus(True, False))
        num.times(BooleanValue.get(Long.MaxValue / 2), False2)
        assertEx(num.times(BooleanValue.get(Long.MaxValue), False2), classOf[ArithmeticException])
        num.toInt(BooleanValue.get(Int.MaxValue))
        assertEx(num.toInt(BooleanValue.get(Int.MaxValue.toLong + 1)), classOf[ArithmeticException])
    }

}
