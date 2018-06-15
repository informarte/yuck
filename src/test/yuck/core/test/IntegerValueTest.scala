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
final class IntegerValueTest extends UnitTest {

    @Test
    def testComparison {

        assertNe(Zero, null)
        assertNe(Zero, False)
        assertEq(Zero, Zero)
        assertEq(Zero, new IntegerValue(0))
        assertNe(Zero, One)
        assertLt(Zero.compare(One), 0)
        assertEq(Zero.compare(Zero), 0)
        assertGt(One.compare(Zero), 0)

        for ((a, b) <- List((0, 0), (0, 1), (1, 0), (1, 1))) {
            val c = if (a == 0) Zero else One
            val d = if (b == 0) Zero else One
            val e = new IntegerValue(a)
            val f = new IntegerValue(b)
            assertEq(a == b, c == d)
            assertEq(a == b, e == f)
            assertEq(c, e)
            assertEq(d, f)
            assertEq(a.compare(b), c.compare(d))
            assertEq(b.compare(a), d.compare(c))
        }

        for ((a, b) <- List((false, 0), (false, 1), (true, 0), (true, 1))) {
            val c = if (a) True else False
            val d = if (b == 0) Zero else One
            assertNe(c, d)
        }

        assertEq(IntegerValue.min(Zero, One), Zero)
        assertEq(IntegerValue.max(Zero, One), One)
    }

    @Test
    def testNumericalOperations {
        assertEq(One + Two, Three)
        assertEq(Three - One, Two)
        assertEq(Two * Three, Six)
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
        IntegerValue.get(Int.MinValue) - Zero
        assertEx(IntegerValue.get(Int.MinValue) - One, classOf[ArithmeticException])
        IntegerValue.get(Int.MaxValue) + Zero
        assertEx(IntegerValue.get(Int.MaxValue) + One, classOf[ArithmeticException])
        IntegerValue.get(Int.MaxValue / 2) * Two
        assertEx(IntegerValue.get(Int.MaxValue) * Two, classOf[ArithmeticException])
        IntegerValue.get(Int.MinValue + 1).abs
        assertEx(IntegerValue.get(Int.MinValue).abs, classOf[ArithmeticException])
        IntegerValue.get(-2) ^ IntegerValue.get(31)
        assertEx(IntegerValue.get(-2) ^ IntegerValue.get(32), classOf[ArithmeticException])
        Two ^ IntegerValue.get(30)
        assertEx(Two ^ IntegerValue.get(31), classOf[ArithmeticException])
        assertEx(IntegerValue.get(Int.MaxValue) ^ IntegerValue.get(Int.MaxValue), classOf[ArithmeticException])
    }

}
