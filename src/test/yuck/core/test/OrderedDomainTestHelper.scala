package yuck.core.test

import scala.collection.Seq

import yuck.core._
import yuck.test.util.OrderingTestHelper
import yuck.util.OrderingFromOrdered
import yuck.util.logging.{FineLogLevel, LazyLogger}

/**
 * @author Michael Marte
 *
 */
class OrderedDomainTestHelper
    [Value <: OrderedValue[Value]]
    (logger: LazyLogger, randomGenerator: RandomGenerator)
    extends DomainTestHelper[Value](logger)
{

    def testOrdering
        [Domain <: OrderedDomain[Value]]
        (testData: Seq[Domain]):
        Unit =
    {
        logger.withRootLogLevel(FineLogLevel) {
            logger.withLogScope("Test data") {
                testData.foreach(item => logger.log(item.toString))
            }
        }
        val helper = new OrderingTestHelper[Domain](randomGenerator)
        val ord = new OrderingFromOrdered[Domain]
        helper.testOrdering(testData, ord)
    }

}
