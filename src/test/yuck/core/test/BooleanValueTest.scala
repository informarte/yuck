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

    @Test
    def testComparison {

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
    def testNegation {
        assertEq(False.not, True)
        assertEq(True.not, False)
    }

    @Test
    def testCasting {
        BooleanValueTraits.dynamicDowncast(False)
        assertEx(BooleanValueTraits.dynamicDowncast(Zero))
    }

}
