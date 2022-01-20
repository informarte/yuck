package yuck.core.test

import scala.collection.Seq

import yuck.core.{OrderedValue, RandomGenerator}
import yuck.test.util.OrderingTestHelper
import yuck.util.OrderingFromOrdered

/**
 * @author Michael Marte
 *
 */
class OrderedValueTestHelper
    [V <: OrderedValue[V]]
    (randomGenerator: RandomGenerator)
    extends ValueTestHelper[V]
{

    def testOrdering(testData: Seq[V]): Unit = {
        val helper = new OrderingTestHelper[V](randomGenerator)
        val ord = new OrderingFromOrdered[V]
        helper.testOrdering(testData, ord)
        for (a <- testData) {
            for (b <- testData) {
                val cmp = ord.compare(a, b)
                assertEq(cmp == 0, a == b)
                assertEq(cmp != 0, a != b)
                assertEq(cmp < 0, a < b)
                assertEq(cmp <= 0, a <= b)
                assertEq(cmp > 0, a > b)
                assertEq(cmp >= 0, a >= b)
            }
        }
    }

}
