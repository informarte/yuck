package yuck.core.test

import scala.collection.Seq

import yuck.core.{OrderedValue, RandomGenerator}
import yuck.util.OrderingFromOrdered
import yuck.util.testing.OrderingTestHelper

/**
 * @author Michael Marte
 *
 */
class OrderedValueTestHelper
    [Value <: OrderedValue[Value]]
    (randomGenerator: RandomGenerator)
    extends ValueTestHelper[Value]
{

    def testOrdering(testData: Seq[Value]): Unit = {
        val helper = new OrderingTestHelper[Value](randomGenerator)
        val ord = new OrderingFromOrdered[Value]
        helper.testOrdering(testData, ord)
    }

}
