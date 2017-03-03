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
class ValueTest extends UnitTest {

    @Test
    def testBooleanValue {

        assertNe(False, null)
        assertNe(Zero, False)
        assertEq(False, False)
        assertEq(False, new BooleanValue(false))
        assertNe(False, True)
        assertLt(False.compare(True), 0)
        assertEq(False.compare(False), 0)
        assertGt(True.compare(False), 0)

        for ((a, b) <- List((false, false), (false, true), (true, false), (true, true))) {
            val c = if (a) True else False
            val d = if (b) True else False
            val e = new BooleanValue(a)
            val f = new BooleanValue(b)
            assertEq(a == b, c == d)
            assertEq(a == b, e == f)
            assertEq(c, e)
            assertEq(d, f)
            assertEq(a.compare(b), c.compare(d))
            assertEq(b.compare(a), d.compare(c))
        }

    }

    @Test
    def testIntegerValue {

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
    def testIntegerSetValue {
        assertEq(EmptyIntegerSet, EmptyIntegerSet)
        assertEq(NonNegativeIntegerSet, NonNegativeIntegerSet)
        assertEq(UnboundedIntegerSet, UnboundedIntegerSet)
        assertNe(EmptyIntegerSet, NonNegativeIntegerSet)
        assertNe(EmptyIntegerSet, UnboundedIntegerSet)
        assertNe(NonNegativeIntegerSet, UnboundedIntegerSet)
        assertLt(EmptyIntegerSet, NonNegativeIntegerSet)
        assertLt(EmptyIntegerSet, UnboundedIntegerSet)
        assertLt(NonNegativeIntegerSet, UnboundedIntegerSet)
        assertGt(NonNegativeIntegerSet, EmptyIntegerSet)
        assertGt(UnboundedIntegerSet, EmptyIntegerSet)
        assertGt(UnboundedIntegerSet, NonNegativeIntegerSet)
        val a = new IntegerSetValue(new IntegerDomain(Zero, Two))
        val b = new IntegerSetValue(new IntegerDomain(One, Three))
        assertLt(a, b)
        assertGt(b, a)
    }

    @Test
    def testPolymorphicListValue {
        type PLV = PolymorphicListValue
        val nil = new PLV(Nil)
        assertNe(nil, null)
        assertNe(nil, False)
        assertEq(nil, nil)
        assertEq(nil, new PLV(Nil))
        assertNe(nil, new PLV(List(One)))
        assertNe(new PLV(List(One, False)), new PLV(List(One)))
        assertEq(new PLV(List(True, False)), new PLV(List(True, False)))
        assertNe(new PLV(List(One, False)), new PLV(List(False, One)))
    }

    @Test
    def testValueCasting {
        BooleanValueTraits.dynamicDowncast(False)
        assertEx(BooleanValueTraits.dynamicDowncast(Zero))
        assertEx(IntegerValueTraits.dynamicDowncast(False))
        IntegerValueTraits.dynamicDowncast(Zero)
    }

}
