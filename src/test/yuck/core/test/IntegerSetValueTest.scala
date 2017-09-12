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

    private val NonNegativeIntegerSetValue = new IntegerSetValue(NonNegativeIntegerRange)

    @Test
    def testComparison {
        assertEq(EmptyIntegerSetValue, EmptyIntegerSetValue)
        assertEq(NonNegativeIntegerSetValue, NonNegativeIntegerSetValue)
        assertEq(CompleteIntegerSetValue, CompleteIntegerSetValue)
        assertNe(EmptyIntegerSetValue, NonNegativeIntegerSetValue)
        assertNe(EmptyIntegerSetValue, CompleteIntegerSetValue)
        assertNe(NonNegativeIntegerSetValue, CompleteIntegerSetValue)
        assertLt(EmptyIntegerSetValue, NonNegativeIntegerSetValue)
        assertLt(EmptyIntegerSetValue, CompleteIntegerSetValue)
        assertLt(NonNegativeIntegerSetValue, CompleteIntegerSetValue)
        assertGt(NonNegativeIntegerSetValue, EmptyIntegerSetValue)
        assertGt(CompleteIntegerSetValue, EmptyIntegerSetValue)
        assertGt(CompleteIntegerSetValue, NonNegativeIntegerSetValue)
        val a = new IntegerSetValue(new IntegerRange(Zero, Two))
        val b = new IntegerSetValue(new IntegerRange(One, Three))
        assertLt(a, b)
        assertGt(b, a)
    }

}
