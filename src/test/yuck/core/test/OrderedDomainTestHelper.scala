package yuck.core.test

import scala.collection.Seq

import yuck.core._

/**
 * @author Michael Marte
 *
 */
class OrderedDomainTestHelper
    [Value <: OrderedValue[Value]]
    (implicit valueTraits: OrderedValueTraits[Value])
    extends DomainTestHelper[Value]
{

    def testOrdering
        [Interface <: OrderedDomain[Value], Implementation <: Interface]
        (testData: Seq[Implementation], ord: Ordering[Interface])
    {
        val helper = new OrderingTestHelper[Interface] {}
        val sortedTestData1 = helper.testOrdering(testData, valueTraits.domainOrdering)
        val sortedTestData2 = helper.testOrdering(testData, ord)
        assertEq(sortedTestData1, sortedTestData2)
    }

}
