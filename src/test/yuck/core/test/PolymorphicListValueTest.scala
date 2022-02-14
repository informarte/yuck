package yuck.core.test

import org.junit.*

import yuck.core.*
import yuck.test.util.UnitTest

/**
 * @author Michael Marte
 *
 */
@FixMethodOrder(runners.MethodSorters.NAME_ASCENDING)
final class PolymorphicListValueTest extends UnitTest {

    private type PLV = PolymorphicListValue

    @Test
    def testComparison: Unit = {
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

}
