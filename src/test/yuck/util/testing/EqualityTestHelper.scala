package yuck.util.testing

import scala.collection._

/**
 * @author Michael Marte
 *
 */
class EqualityTestHelper[T] extends YuckAssert {

    // Checks that each given value equals itself and that the given values differ from each other.
    def testEquality(testData: Seq[T]) {
        for (a <- testData) {
            assertEq(a, a)
            assertNe(a, null)
            assertNe(null, a)
        }
        for (Seq(a, b) <- testData.combinations(2)) {
            assertNe(a, b)
            assertNe(b, a)
        }
    }

}
