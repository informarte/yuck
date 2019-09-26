package yuck.util.testing

import yuck.core.RandomGenerator

import scala.collection._

/**
 * @author Michael Marte
 *
 */
class OrderingTestHelper[T](randomGenerator: RandomGenerator) extends EqualityTestHelper[T] {

    // compare induces a couple of relations: =, <=, >=, <, >
    // We check that:
    // = is an equivalence relation in sync with equals
    // <= is a total order
    // >= is the inverse of <=
    // > is the inverse of <
    // For efficiency, testData should not contain duplicates.
    def testOrdering(testData: Seq[T], ord: Ordering[U] forSome {type U >: T}): Unit = {
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
        for (Seq(a, b) <- randomGenerator.shuffle(testData ++ testData).sorted(ord).combinations(2)) {
            assertLe(ord.compare(a, b), 0)
        }
    }

}
