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

    @Test
    def testComparison {

        assertNe(Zero, null)
        assertNe(Zero, False)

        val testRange = -5 to 5
        for (a <- testRange) {
            val c = IntegerValue.get(a)
            assertEq(c.value, a)
            val e = new IntegerValue(a)
            assertEq(c, e)
            for (b <- testRange) {
                val d = IntegerValue.get(b)
                assertEq(d.value, b)
                val f = new IntegerValue(b)
                assertEq(d, f)
                assertEq(a == b, c == d)
                assertEq(a == b, e == f)
                assertEq(a != b, c != d)
                assertEq(signum(a.compare(b)), signum(c.compare(d)))
                assertEq(signum(b.compare(a)), signum(d.compare(c)))
                assertEq(c.eqc(d).truthValue, c == d)
                assertEq(c.nec(d).truthValue, c != d)
                assertEq(c.ltc(d).truthValue, c < d)
                assertEq(c.lec(d).truthValue, c <= d)
            }
        }

        assertEq(IntegerValue.min(Zero, One), Zero)
        assertEq(IntegerValue.max(Zero, One), One)

    }

    @Test
    def testNumericalOperations {
        assertEq(One + Two, Three)
        assertEq(Three - One, Two)
        assertEq(Two * Three, Six)
        assertEq(One.addAndSub(Two, Five, Two), Seven)
        assertEq(One.addAndSub(One, IntegerValue.get(Int.MaxValue), IntegerValue.get(Int.MaxValue - 1)), Two)
        assertEq(Six / Two, Three)
        assertEq(Two ^ Three, Eight)
        assertEq(Seven % Two, One)
        assertEq(MinusOne.abs, One)
        assertEq(MinusOne.toDouble, -1.0)
        assert(Zero.isEven)
        assert(! One.isEven)
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
