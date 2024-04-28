package yuck.core.test

import org.junit.*

import yuck.core.{given, *}
import yuck.test.*
import yuck.test.util.UnitTest

/**
 * @author Michael Marte
 *
 */
@FixMethodOrder(runners.MethodSorters.NAME_ASCENDING)
final class IntegerValueTest extends UnitTest with IntegerValueTestData {

    private val randomGenerator = new JavaRandomGenerator
    private val helper = new OrderedValueTestHelper[IntegerValue](randomGenerator)

    @Test
    def testConstruction(): Unit = {
        for (a <- testRange) {
            assertEq(new IntegerValue(a).toInt, a)
        }
    }

    @Test
    def testSpecialValues(): Unit = {
        assertEq(MinusOne.value, -1L)
        assertEq(Zero.value, 0L)
        assertEq(One.value, 1L)
        assertEq(Two.value, 2L)
        assertEq(Three.value, 3L)
        assertEq(Four.value, 4L)
        assertEq(Five.value, 5L)
        assertEq(Six.value, 6L)
        assertEq(Seven.value, 7L)
        assertEq(Eight.value, 8L)
        assertEq(Nine.value, 9L)
        assertEq(Ten.value, 10L)
    }

    @Test
    def testValueFactory(): Unit = {
        for (a <- testRange) {
            assertEq(IntegerValue(a).toInt, a)
            assert(IntegerValue(a).eq(IntegerValue(a)))
        }
    }

    @Test
    def testEquality(): Unit = {
        helper.testEquality(testData)
        for (a <- testData) {
            val b = new IntegerValue(a.value)
            assertEq(a, b)
            assertEq(b, a)
            assertNe(a, False)
            assertNe(False, a)
            for (b <- testData) {
                assert(if (a.eq(b)) a == b else a != b)
            }
        }
    }

    @Test
    def testOrdering(): Unit = {
        helper.testOrdering(testData)
        for (a <- testData) {
            for (b <- testData) {
                assertEq(a.compare(b).sign, a.value.compare(b.value).sign)
            }
        }
        assertEq(IntegerValue.min(Zero, One), Zero)
        assertEq(IntegerValue.max(Zero, One), One)
    }

    @Test
    def testNumericalOperations(): Unit = {
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
                    assertEq((a ^ b).value, scala.math.pow(a.value.toDouble, b.value.toDouble).toLong)
                }
                for (c <- testData) {
                    assertEq(a.addAndSub(b, c).value, a.value + b.value - c.value)
                    for (d <- testData) {
                        assertEq(a.addAndSub(b, c, d).value, a.value + b.value * (c.value - d.value))
                    }
                }
            }
            if (a.value < 0) {
                assertEq(a.abs, IntegerValue(-a.value))
            } else {
                assertEq(a.abs, a)
            }
            assertEq(a.negated, IntegerValue(-a.value))
            assertEq(a.toInt, a.value.toInt)
            assertEq(a.toLong, a.value)
            assertEq(a.toFloat, a.value.toFloat)
            assertEq(a.toDouble, a.value.toDouble)
            assertEq(a.isEven, a.value % 2 == 0)
        }
    }

    @Test
    def testOverflowCheckingInNumericalOperations(): Unit = {
        IntegerValue(Long.MaxValue) + Zero
        assertEx(IntegerValue(Long.MaxValue) + One, classOf[ArithmeticException])
        IntegerValue(Long.MinValue) - Zero
        assertEx(IntegerValue(Long.MinValue) - One, classOf[ArithmeticException])
        IntegerValue(Long.MaxValue / 2) * Two
        assertEx(IntegerValue(Long.MaxValue) * Two, classOf[ArithmeticException])
        IntegerValue(Long.MaxValue - 1).addAndSub(One, Zero)
        One.addAndSub(IntegerValue(Long.MaxValue), IntegerValue(Long.MaxValue - 1))
        IntegerValue(Long.MaxValue - 1).addAndSub(One, One, Zero)
        One.addAndSub(One, IntegerValue(Long.MaxValue), IntegerValue(Long.MaxValue - 1))
        assertEx(IntegerValue(Long.MaxValue).addAndSub(One, One, Zero), classOf[ArithmeticException])
        assertEq(IntegerValue(Long.MinValue + 1).abs.value, Long.MaxValue)
        assertEx(IntegerValue(Long.MinValue).abs, classOf[ArithmeticException])
        assertEq(IntegerValue(Long.MaxValue).negated.value, Long.MinValue + 1)
        assertEx(IntegerValue(Long.MinValue).negated, classOf[ArithmeticException])
        assertEq((IntegerValue(-2) ^ IntegerValue(63)).value, Long.MinValue)
        assertEx(IntegerValue(-2) ^ IntegerValue(64), classOf[ArithmeticException])
        assertEq((Two ^ IntegerValue(63)).value, Long.MaxValue)
        assertEx(Two ^ IntegerValue(64), classOf[ArithmeticException])
        assertEx(IntegerValue(Long.MaxValue) ^ IntegerValue(Long.MaxValue), classOf[ArithmeticException])
    }

    @Test
    def testConfiguration(): Unit = {
        import IntegerValue.{given}
        assertEq(operations, IntegerValueOperations)
        assertEq(traits, IntegerValueTraits)
    }

}
