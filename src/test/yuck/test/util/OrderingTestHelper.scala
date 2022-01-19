package yuck.test.util

import scala.collection._

import yuck.core.RandomGenerator

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
    def testOrdering(testData: Seq[T], ord: Ordering[T]): Unit = {
        for (a <- testData) {
            // reflexivity of =
            assertEq(ord.compare(a, a), 0)
        }
        for (a <- testData) {
            for (b <- testData) {
                val cmp1 = ord.compare(a, b)
                val cmp2 = ord.compare(b, a)
                // symmetry of =
                assertEq(cmp1 == 0, cmp2 == 0)
                // consistency of = with equals
                assertEq(cmp1 == 0, a == b)
                // antisymmetry of <=
                if (cmp1 <= 0 && cmp2 <= 0) {
                    assertEq(cmp1, 0)
                }
                // > is the inverse of <
                assertEq(cmp1 < 0, cmp2 > 0)
                // >= is the inverse of <=
                assertEq(cmp1 <= 0, cmp2 >= 0)
                for (c <- testData) {
                    // transitivity of <=
                    if (cmp1 <= 0 && ord.compare(b, c) <= 0) {
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
