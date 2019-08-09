package yuck.core.test

import yuck.core._
import yuck.util.logging.LazyLogger
import yuck.util.testing.OrderingTestHelper

/**
 * @author Michael Marte
 *
 */
class OrderedDomainTestHelper
    [Value <: OrderedValue[Value]]
    (logger: LazyLogger)
    (implicit valueTraits: OrderedValueTraits[Value])
    extends DomainTestHelper[Value](logger)
{

    def testOrdering
        [Interface <: OrderedDomain[Value], Implementation <: Interface]
        (testData: Seq[Implementation], ord: Ordering[Interface]): Unit =
    {
        logger.withLogScope("Test data") {
            testData.foreach(item => logger.log(item.toString))
        }
        val helper = new OrderingTestHelper[Interface]
        val sortedTestData1 = helper.testOrdering(testData, valueTraits.domainOrdering)
        val sortedTestData2 = helper.testOrdering(testData, ord)
        assertEq(sortedTestData1, sortedTestData2)
    }

}
