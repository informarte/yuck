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
    def testConstruction {
        for (a <- testRange) {
            assertEq(new IntegerValue(a).value, a)
        }
    }

    @Test
    def testSpecialValues {
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
    def testValueFactory {
        for (a <- testRange) {
            assertEq(IntegerValue.get(a).value, a)
            assert(IntegerValue.get(a).eq(IntegerValue.get(a)))
        }
    }

    @Test
    def testEquality {
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
    def testOrdering {
        helper.testOrdering(testData ++ testData)
        assertEq(IntegerValue.min(Zero, One), Zero)
        assertEq(IntegerValue.max(Zero, One), One)
    }

    @Test
    def testConstraints {
        for (a <- testData) {
            for (b <- testData) {
                assertEq(a.eqc(b).truthValue, a == b)
                assertEq(a.nec(b).truthValue, a != b)
                assertEq(a.ltc(b).truthValue, a < b)
                assertEq(a.lec(b).truthValue, a <= b)
                for (c <- testData) {
                    if (a < b && a < c && b < c) {
                        assertLe((a eqc b).violation, (a eqc c).violation)
                        assertLe((a ltc b).violation, (a ltc c).violation)
                    }
                }
            }
        }
    }

    @Test
    def testNumericalOperations {
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
                    for (d <- testData) {
                        assertEq(a.addAndSub(b, c, d).value, a.value + b.value * (c.value - d.value))
                    }
                }
            }
            assertEq(a.toInt, a.value)
            assertEq(a.toLong, a.value.toLong)
            assertEq(a.toDouble, a.value.toDouble)
            assertEq(a.isEven, a.value % 2 == 0)
        }
    }

    @Test
    def testOverflowChecking {
        IntegerValue.get(Int.MaxValue) + Zero
        assertEx(IntegerValue.get(Int.MaxValue) + One, classOf[ArithmeticException])
        IntegerValue.get(Int.MinValue) - Zero
        assertEx(IntegerValue.get(Int.MinValue) - One, classOf[ArithmeticException])
        IntegerValue.get(Int.MaxValue / 2) * Two
        assertEx(IntegerValue.get(Int.MaxValue) * Two, classOf[ArithmeticException])
        IntegerValue.get(Int.MaxValue - 1).addAndSub(One, One, Zero)
        One.addAndSub(One, IntegerValue.get(Int.MaxValue), IntegerValue.get(Int.MaxValue - 1))
        assertEx(IntegerValue.get(Int.MaxValue).addAndSub(One, One, Zero), classOf[ArithmeticException])
        IntegerValue.get(Int.MinValue + 1).abs
        assertEx(IntegerValue.get(Int.MinValue).abs, classOf[ArithmeticException])
        IntegerValue.get(-2) ^ IntegerValue.get(31)
        assertEx(IntegerValue.get(-2) ^ IntegerValue.get(32), classOf[ArithmeticException])
        Two ^ IntegerValue.get(30)
        assertEx(Two ^ IntegerValue.get(31), classOf[ArithmeticException])
        assertEx(IntegerValue.get(Int.MaxValue) ^ IntegerValue.get(Int.MaxValue), classOf[ArithmeticException])
        IntegerValue.get(Int.MaxValue - 1).ltc(Zero)
        assertEx(IntegerValue.get(Int.MaxValue).ltc(Zero), classOf[ArithmeticException])
        IntegerValue.get(Int.MaxValue).lec(Zero)
        assertEx(IntegerValue.get(Int.MaxValue).lec(MinusOne), classOf[ArithmeticException])
    }

}
