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
final class BooleanValueTest extends UnitTest with BooleanValueTestData {

    private val randomGenerator = new JavaRandomGenerator
    private val helper = new OrderedValueTestHelper[BooleanValue](randomGenerator)

    @Test
    def testConstruction: Unit = {
        for (a <- testRange) {
            assertEq(new BooleanValue(a).violation, a)
        }
        assertEx(new BooleanValue(-1))
    }

    @Test
    def testSpecialValues: Unit = {
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
    def testValueFactory: Unit = {
        assertEq(BooleanValue.get(false), False)
        assertEq(BooleanValue.get(true), True)
        for (a <- testRange) {
            assertEq(BooleanValue.get(a).violation, a)
            assert(BooleanValue.get(a).eq(BooleanValue.get(a)))
        }
        assertEx(new BooleanValue(-1))
    }

    @Test
    def testEquality: Unit = {
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
    def testOrdering: Unit = {
        helper.testOrdering(testData)
        for (a <- testData) {
            for (b <- testData) {
                assertEq(a.compare(b).sign, a.violation.compare(b.violation).sign)
            }
        }
    }

    @Test
    def testNumericalOperations: Unit = {
        for (a <- testData) {
            for (b <- testData) {
                assertEq((a + b).violation, a.violation + b.violation)
                if (b.violation > a.violation) {
                    assertEx(a - b)
                } else {
                    assertEq((a - b).violation, a.violation - b.violation)
                }
                assertEq((a * b).violation, a.violation * b.violation)
                assertEx(a ^ b, classOf[NotImplementedError])
                for (c <- testData) {
                    val result = a.violation + b.violation - c.violation
                    if (result < 0) {
                        assertEx(a.addAndSub(b, c))
                    } else {
                        assertEq(a.addAndSub(b, c).violation, result)
                    }
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
            assertEq(a.abs, a)
            assertEx(a.negate)
            assertEq(a.toInt, a.violation.toInt)
            assertEq(a.toLong, a.violation.toLong)
            assertEq(a.toFloat, a.violation.toFloat)
            assertEq(a.toDouble, a.violation.toDouble)
        }
    }

    @Test
    def testOverflowCheckingInNumericalOperations: Unit = {
        BooleanValue.get(Long.MaxValue) + True
        assertEx(BooleanValue.get(Long.MaxValue) + False, classOf[ArithmeticException])
        True - True
        assertEx(True - False)
        BooleanValue.get(Long.MaxValue / 2) * False2
        assertEx(BooleanValue.get(Long.MaxValue) * False2, classOf[ArithmeticException])
        BooleanValue.get(Long.MaxValue - 1).addAndSub(False, True)
        assertEx(BooleanValue.get(Long.MaxValue).addAndSub(False, True), classOf[ArithmeticException])
        BooleanValue.get(Long.MaxValue - 1).addAndSub(False, False, True)
        assertEx(BooleanValue.get(Long.MaxValue).addAndSub(False, False, True), classOf[ArithmeticException])
        BooleanValue.get(Int.MaxValue).toInt
        assertEx(BooleanValue.get(Int.MaxValue.toLong + 1).toInt, classOf[ArithmeticException])
    }

    @Test
    def testNegation: Unit = {
        assertEq(False.not, True)
        assertEq(False2.not, True)
        assertEq(True.not, False)
    }

    @Test
    def testConfiguration: Unit = {
        import BooleanValue._
        assertEq(valueTraits, BooleanValueTraits)
        assertEq(numericalOperations, BooleanValueOperations)
        assertEq(domainOrdering, BooleanDomainOrdering)
    }

}
