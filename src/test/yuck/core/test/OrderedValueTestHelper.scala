package yuck.core.test

import yuck.core.{OrderedValue, OrderedValueTraits}
import yuck.util.testing.OrderingTestHelper

/**
 * @author Michael Marte
 *
 */
class OrderedValueTestHelper
    [Value <: OrderedValue[Value]]
    (implicit valueTraits: OrderedValueTraits[Value])
    extends ValueTestHelper[Value]
{
    def testOrdering(testData: Seq[Value]): Unit = {
        val helper = new OrderingTestHelper[Value]
        val sortedTestData1 = helper.testOrdering(testData, valueTraits.valueOrdering)
        val ord = new Ordering[Value] {
            override def compare(a: Value, b: Value) = a.compare(b)
        }
        val sortedTestData2 = helper.testOrdering(testData, ord)
        assertEq(sortedTestData1, sortedTestData2)
    }
}