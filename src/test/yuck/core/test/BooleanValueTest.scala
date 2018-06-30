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
final class BooleanValueTest extends UnitTest {

    @Test
    def testComparison {

        assertNe(False, null)
        assertNe(Zero, False)

        assertLt(True, False)

        val testRange = 0 to 5
        for (a <- testRange) {
            val c = BooleanValue.get(a)
            assertEq(c.violation, a)
            val e = new BooleanValue(a)
            assertEq(c, e)
            for (b <- testRange) {
                val d = BooleanValue.get(b)
                assertEq(d.violation, b)
                val f = new BooleanValue(b)
                assertEq(d, f)
                assertEq(a == b, c == d)
                assertEq(a == b, e == f)
                assertEq(a != b, c != d)
                assertEq(signum(a.compare(b)), signum(c.compare(d)))
                assertEq(signum(b.compare(a)), signum(d.compare(c)))
                assertEq(c.eqc(d).truthValue, c.truthValue == d.truthValue)
                assertEq(c.nec(d).truthValue, c.truthValue != d.truthValue)
                assertEq(c.ltc(d).truthValue, ! c.truthValue && d.truthValue)
                assertEq(c.lec(d).truthValue, ! c.truthValue || d.truthValue)
            }
        }

    }

    @Test
    def testNegation {
        assertEq(False.not, True)
        assertEq(False2.not, True)
        assertEq(True.not, False)
    }

    @Test
    def testNumericalOperations {
        assertEq(False + False2, False3)
        assertEq(False3 - False, False2)
        assertEq(False.addAndSub(False2, False5, False2), False7)
        assertEq(False2 * False3, False6)
        assertEq(False.addAndSub(False2, False5, False2), False7)
        assertEx(False6 / False2, classOf[NotImplementedError])
        assertEx(False2 ^ False3, classOf[NotImplementedError])
        assertEx(False7 % False2, classOf[NotImplementedError])
        assertEx(False.abs, classOf[NotImplementedError])
        assertEq(False.toDouble, 1.0)
        assertEx(True.isEven, classOf[NotImplementedError])
    }

    @Test
    def testOverflowChecking {
        assertEx(new BooleanValue(-1), classOf[IllegalArgumentException])
        BooleanValue.get(Int.MaxValue) + True
        assertEx(BooleanValue.get(Int.MaxValue) + False, classOf[ArithmeticException])
        True - True
        assertEx(True - False, classOf[IllegalArgumentException])
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
