package yuck.core.test.util

import scala.collection.*

import yuck.test.util.YuckAssert

trait EqualityTestTooling[T <: AnyRef] extends YuckAssert {

    // Checks that each given value equals itself and that the given values differ from each other.
    def testEquality(testData: Seq[T]): Unit = {
        for a <- testData do {
            assertEq(a, a)
            assertNe(a, null)
            assertNe(null, a)
            for b <- testData do {
                if a.eq(b) then {
                    assertEq(a, b)
                    assertEq(b, a)
                } else {
                    assertNe(a, b)
                    assertNe(b, a)
                }
            }
        }
    }

}
