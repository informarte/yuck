package yuck.core.test

import scala.math.{abs, signum}

import org.junit._

import yuck.core._
import yuck.util.testing.UnitTest

/**
 * @author Michael Marte
 *
 */
@Test
@FixMethodOrder(runners.MethodSorters.NAME_ASCENDING)
final class IntegerValueTest extends UnitTest {

    private val helper = new OrderedValueTestHelper[IntegerValue]
    private val testRange = -5 to 5
    private val testData = testRange.map(IntegerValue.get)

    @Test
    def testConstruction: Unit = {
        for (a <- testRange) {
            assertEq(new IntegerValue(a).value, a)
        }
    }

    @Test
    def testSpecialValues: Unit = {
        assertEq(MinusOne.value, -1)
        assertEq(Zero.value, 0)
        assertEq(One.value, 1)
        assertEq(Two.value, 2)
        assertEq(Three.value, 3)
        assertEq(Four.value, 4)
        assertEq(Five.value, 5)
        assertEq(Six.value, 6)
        assertEq(Seven.value, 7)
        assertEq(Eight.value, 8)
        assertEq(Nine.value, 9)
        assertEq(Ten.value, 10)
    }

    @Test
    def testValueFactory: Unit = {
        for (a <- testRange) {
            assertEq(IntegerValue.get(a).value, a)
            assert(IntegerValue.get(a).eq(IntegerValue.get(a)))
        }
    }

    @Test
    def testEquality: Unit = {
        helper.testEquality(testData)
        for (a <- testData) {
            val b = new IntegerValue(a.value)
            assertEq(a, b)
            assertEq(b, a)
            assertNe(a, False)
            assertNe(False, a)
        }
    }

    @Test
    def testOrdering: Unit = {
        helper.testOrdering(testData ++ testData)
        assertEq(IntegerValue.min(Zero, One), Zero)
        assertEq(IntegerValue.max(Zero, One), One)
    }

    @Test
    def testOrderingCostModel: Unit = {
        val costModel = IntegerOrderingCostModel
        for (a <- testData) {
            for (b <- testData) {
                assertEq(costModel.eq(a, b).truthValue, a == b)
                assertEq(costModel.ne(a, b).truthValue, a != b)
                assertEq(costModel.lt(a, b).truthValue, a < b)
                assertEq(costModel.le(a, b).truthValue, a <= b)
                for (c <- testData) {
                    if (a < b && a < c && b < c) {
                        assertLe(costModel.eq(a, b).violation, costModel.eq(a, c).violation)
                        assertLe(costModel.lt(a, b).violation, costModel.lt(a, c).violation)
                    }
                }
            }
        }
    }

    @Test
    def testOverflowCheckingInCostComputation: Unit = {
        val costModel = IntegerOrderingCostModel
        costModel.lt(IntegerValue.get(Int.MaxValue - 1), Zero)
        assertEx(costModel.lt(IntegerValue.get(Int.MaxValue), Zero), classOf[ArithmeticException])
        costModel.le(IntegerValue.get(Int.MaxValue), Zero)
        assertEx(costModel.le(IntegerValue.get(Int.MaxValue), MinusOne), classOf[ArithmeticException])
    }

    @Test
    def testNumericalOperations: Unit = {
        for (a <- testData) {
            for (b <- testData) {
                assertEq((a + b).value, a.value + b.value)
                assertEq((a - b).value, a.value - b.value)
                assertEq((a * b).value, a.value * b.value)
                if (b == Zero) {
                    assertEx(a / b, classOf[ArithmeticException])
                    assertEx(a % b, classOf[ArithmeticException])
                } else {
                    assertEq((a / b).value, a.value / b.value)
                    assertEq((a % b).value, a.value % b.value)
                }
                if (a == Zero && b < Zero) {
                    assertEx(a ^ b, classOf[ArithmeticException])
                } else {
                    assertEq((a ^ b).value, scala.math.pow(a.value, b.value).toInt)
                }
                for (c <- testData) {
                    assertEq(a.addAndSub(b, c).value, a.value + b.value - c.value)
                    for (d <- testData) {
                        assertEq(a.addAndSub(b, c, d).value, a.value + b.value * (c.value - d.value))
                    }
                }
            }
            if (a.value < 0) {
                assertEq(a.abs, IntegerValue.get(-a.value))
            } else {
                assertEq(a.abs, a)
            }
            assertEq(a.neg, IntegerValue.get(-a.value))
            assertEq(a.toInt, a.value)
            assertEq(a.toLong, a.value.toLong)
            assertEq(a.toDouble, a.value.toDouble)
            assertEq(a.isEven, a.value % 2 == 0)
        }
    }

    @Test
    def testOverflowCheckingInNumericalOperations: Unit = {
        IntegerValue.get(Int.MaxValue) + Zero
        assertEx(IntegerValue.get(Int.MaxValue) + One, classOf[ArithmeticException])
        IntegerValue.get(Int.MinValue) - Zero
        assertEx(IntegerValue.get(Int.MinValue) - One, classOf[ArithmeticException])
        IntegerValue.get(Int.MaxValue / 2) * Two
        assertEx(IntegerValue.get(Int.MaxValue) * Two, classOf[ArithmeticException])
        IntegerValue.get(Int.MaxValue - 1).addAndSub(One, Zero)
        One.addAndSub(IntegerValue.get(Int.MaxValue), IntegerValue.get(Int.MaxValue - 1))
        IntegerValue.get(Int.MaxValue - 1).addAndSub(One, One, Zero)
        One.addAndSub(One, IntegerValue.get(Int.MaxValue), IntegerValue.get(Int.MaxValue - 1))
        assertEx(IntegerValue.get(Int.MaxValue).addAndSub(One, One, Zero), classOf[ArithmeticException])
        IntegerValue.get(Int.MinValue + 1).abs
        assertEx(IntegerValue.get(Int.MinValue).abs, classOf[ArithmeticException])
        IntegerValue.get(Int.MaxValue).neg
        assertEx(IntegerValue.get(Int.MinValue).neg, classOf[ArithmeticException])
        IntegerValue.get(-2) ^ IntegerValue.get(31)
        assertEx(IntegerValue.get(-2) ^ IntegerValue.get(32), classOf[ArithmeticException])
        Two ^ IntegerValue.get(30)
        assertEx(Two ^ IntegerValue.get(31), classOf[ArithmeticException])
        assertEx(IntegerValue.get(Int.MaxValue) ^ IntegerValue.get(Int.MaxValue), classOf[ArithmeticException])
    }

}
