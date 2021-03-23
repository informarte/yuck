package yuck.core.test

import org.junit._

import yuck.core._
import yuck.test.util.{OrderingTestHelper, UnitTest}

/**
 * @author Michael Marte
 *
 */
@FixMethodOrder(runners.MethodSorters.NAME_ASCENDING)
final class IntegerValueOperationsTest extends UnitTest with IntegerValueTestData {

    private val num = IntegerValueOperations
    private val randomGenerator = new JavaRandomGenerator

    @Test
    def testValueFactory: Unit = {
        for (a <- testRange) {
            assertEq(num.fromInt(a).value, a)
        }
    }

    @Test
    def testSpecialValues: Unit = {
        assertEq(num.zero, Zero)
        assertEq(num.one, One)
    }

    @Test
    def testOrdering: Unit = {
        val helper = new OrderingTestHelper[IntegerValue](randomGenerator)
        helper.testOrdering(testData, num)
        val ord = new Ordering[IntegerValue] {
            override def compare(a: IntegerValue, b: IntegerValue) = a.compare(b)
        }
        assertEq(testData.sorted(num), testData.sorted(ord))
    }

    @Test
    def testNumericalOperations: Unit = {
        for (a <- testData) {
            for (b <- testData) {
                assertEq(num.plus(a, b), a + b)
                assertEq(num.minus(a, b), a - b)
                assertEq(num.times(a, b), a * b)
                if (b == Zero) {
                    assertEx(num.quot(a, b), classOf[ArithmeticException])
                    assertEx(num.rem(a,  b), classOf[ArithmeticException])
                } else {
                    assertEq(num.quot(a, b), a / b)
                    assertEq(num.rem(a, b), a % b)
                }
            }
            if (a.value < 0) {
                assertEq(num.abs(a), a.abs)
            } else {
                assertEq(num.abs(a), a)
            }
            assertEq(num.negate(a), a.negate)
            assertEq(num.toInt(a), a.toInt)
            assertEq(num.toLong(a), a.toLong)
            assertEq(num.toFloat(a), a.toFloat)
            assertEq(num.toDouble(a), a.toDouble)
        }
    }

    @Test
    def testOverflowCheckingInNumericalOperations: Unit = {
        num.plus(IntegerValue(Int.MaxValue), Zero)
        assertEx(num.plus(IntegerValue(Int.MaxValue), One), classOf[ArithmeticException])
        num.minus(IntegerValue(Int.MinValue), Zero)
        assertEx(num.minus(IntegerValue(Int.MinValue), One), classOf[ArithmeticException])
        num.times(IntegerValue(Int.MaxValue / 2), Two)
        assertEx(num.times(IntegerValue(Int.MaxValue), Two), classOf[ArithmeticException])
        num.abs(IntegerValue(Int.MinValue + 1))
        assertEx(num.abs(IntegerValue(Int.MinValue)), classOf[ArithmeticException])
        num.negate(IntegerValue(Int.MaxValue))
        assertEx(num.negate(IntegerValue(Int.MinValue)), classOf[ArithmeticException])
    }

}
