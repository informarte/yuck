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
final class IntegerSetValueTest extends UnitTest {

    @Test
    def testComparison {
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
    def testCasting {
        IntegerSetValueTraits.dynamicDowncast(EmptyIntegerSet)
        assertEx(IntegerSetValueTraits.dynamicDowncast(Zero))
    }

}
