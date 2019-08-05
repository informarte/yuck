package yuck.util.testing

import scala.collection._

/**
 * @author Michael Marte
 *
 */
class OrderingTestHelper[T] extends EqualityTestHelper[T] {

    // compare induces a couple of relations: =, <=, >=, <, >
    // We check that:
    // = is an equivalence relation in sync with equals
    // <= is a total order
    // >= is the inverse of <=
    // > is the inverse of <
    def testOrdering(testData: Seq[T], ord: Ordering[U] forSome {type U >: T}): Seq[T] = {
        for (a <- testData) {
            // reflexivity of =
            assertEq(ord.compare(a, a), 0)
        }
        for (a <- testData) {
            for (b <- testData) {
                // symmetry of =
                assertEq(ord.compare(a, b) == 0, ord.compare(b, a) == 0)
                // consistency of = with equals
                assertEq(ord.compare(a, b) == 0, a.equals(b))
                // antisymmetry of <=
                if (ord.compare(a, b) <= 0 && ord.compare(b, a) <= 0) {
                    assertEq(ord.compare(a, b), 0)
                }
                // > is the inverse of <
                assertEq(ord.compare(a, b) < 0, ord.compare(b, a) > 0)
                // >= is the inverse of <=
                assertEq(ord.compare(a, b) <= 0, ord.compare(b, a) >= 0)
                for (c <- testData) {
                    // transitivity of <=
                    if (ord.compare(a, b) <= 0 && ord.compare(b, c) <= 0) {
                        assertLe(ord.compare(a, c), 0)
                    }
                }
            }
        }
        val sortedTestData = testData.sorted(ord)
        for (Seq(a, b) <- sortedTestData.combinations(2)) {
            assertLe(ord.compare(a, b), 0)
        }
        sortedTestData
    }

}
