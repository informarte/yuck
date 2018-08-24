package yuck.core.test

import org.junit._

import yuck.core._
import yuck.util.testing.UnitTest

/**
 * @author Michael Marte
 *
 */
@Test
@FixMethodOrder(runners.MethodSorters.NAME_ASCENDING)
final class BooleanValueTest extends UnitTest {

    private val helper = new OrderedValueTestHelper[BooleanValue]
    private val testRange = 0 to 5
    private val testData = testRange.map(BooleanValue.get)

    @Test
    def testConstruction {
        for (a <- testRange) {
            assertEq(new BooleanValue(a).violation, a)
        }
        assertEx(new BooleanValue(-1))
    }

    @Test
    def testSpecialValues {
        assertEq(True.violation, 0)
        assertEq(False.violation, 1)
        assertEq(False2.violation, 2)
        assertEq(False3.violation, 3)
        assertEq(False4.violation, 4)
        assertEq(False5.violation, 5)
        assertEq(False6.violation, 6)
        assertEq(False7.violation, 7)
        assertEq(False8.violation, 8)
        assertEq(False9.violation, 9)
        assertEq(False10.violation, 10)
    }

    @Test
    def testValueFactory {
        for (a <- testRange) {
            assertEq(BooleanValue.get(a).violation, a)
            assert(BooleanValue.get(a).eq(BooleanValue.get(a)))
        }
    }

    @Test
    def testEquality {
        helper.testEquality(testData)
        for (a <- testData) {
            val b = new BooleanValue(a.violation)
            assertEq(a, b)
            assertEq(b, a)
            assertNe(a, Zero)
            assertNe(Zero, a)
        }
    }

    @Test
    def testOrdering {
        helper.testOrdering(testData ++ testData)
    }

    @Test
    def testConstraints {
        for (a <- testData) {
            for (b <- testData) {
                assertEq(a.eqc(b).truthValue, a.truthValue == b.truthValue)
                assertEq(a.nec(b).truthValue, a.truthValue != b.truthValue)
                assertEq(a.ltc(b).truthValue, ! a.truthValue && b.truthValue)
                assertEq(a.lec(b).truthValue, ! a.truthValue || b.truthValue)
            }
        }
    }

    @Test
    def testNumericalOperations {
        for (a <- testData) {
            for (b <- testData) {
                assertEq((a + b).violation, a.violation + b.violation)
                if (b.violation > a.violation) {
                    assertEx(a - b)
                } else {
                    assertEq((a - b).violation, a.violation - b.violation)
                }
                assertEq((a * b).violation, a.violation * b.violation)
                assertEx(a / b, classOf[NotImplementedError])
                assertEx(a % b, classOf[NotImplementedError])
                assertEx(a ^ b, classOf[NotImplementedError])
                for (c <- testData) {
                    for (d <- testData) {
                        val result = a.violation + b.violation * (c.violation - d.violation)
                        if (result < 0) {
                            assertEx(a.addAndSub(b, c, d))
                        } else {
                            assertEq(a.addAndSub(b, c, d).violation, result)
                        }
                    }
                }
            }
            assertEq(a.toDouble, a.violation.toDouble)
            assertEx(a.isEven, classOf[NotImplementedError])
        }
    }

    @Test
    def testNegation {
        assertEq(False.not, True)
        assertEq(False2.not, True)
        assertEq(True.not, False)
    }

    @Test
    def testOverflowChecking {
        assertEx(new BooleanValue(-1))
        BooleanValue.get(Int.MaxValue) + True
        assertEx(BooleanValue.get(Int.MaxValue) + False, classOf[ArithmeticException])
        True - True
        assertEx(True - False)
        BooleanValue.get(Int.MaxValue / 2) * False2
        assertEx(BooleanValue.get(Int.MaxValue) * False2, classOf[ArithmeticException])
        BooleanValue.get(Int.MaxValue - 1).addAndSub(False, False, True)
        assertEx(BooleanValue.get(Int.MaxValue).addAndSub(False, False, True), classOf[ArithmeticException])
        True.eqc(BooleanValue.get(Int.MaxValue - 1))
        assertEx(True.eqc(BooleanValue.get(Int.MaxValue)), classOf[ArithmeticException])
        False.nec(BooleanValue.get(Int.MaxValue - 1))
        assertEx(False.nec(BooleanValue.get(Int.MaxValue)), classOf[ArithmeticException])
        True.ltc(BooleanValue.get(Int.MaxValue - 1))
        assertEx(True.ltc(BooleanValue.get(Int.MaxValue)), classOf[ArithmeticException])
        True.lec(BooleanValue.get(Int.MaxValue - 1))
        assertEx(True.lec(BooleanValue.get(Int.MaxValue)), classOf[ArithmeticException])
    }

}
