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
    [V <: OrderedValue[V]]
    (logger: LazyLogger, randomGenerator: RandomGenerator)
    extends DomainTestHelper[V](logger)
{

    def testOrdering(testData: Seq[OrderedDomain[V]]): Unit = {
        logger.withRootLogLevel(FineLogLevel) {
            logger.withLogScope("Test data") {
                testData.foreach(item => logger.log(item.toString))
            }
        }
        val helper = new OrderingTestHelper[OrderedDomain[V]](randomGenerator)
        val ord = new OrderingFromOrdered[OrderedDomain[V]]
        helper.testOrdering(testData, ord)
        for (d <- testData) {
            for (e <- testData) {
                val cmp = ord.compare(d, e)
                assertEq(cmp == 0, d == e)
                assertEq(cmp != 0, d != e)
                assertEq(cmp < 0, d < e)
                assertEq(cmp <= 0, d <= e)
                assertEq(cmp > 0, d > e)
                assertEq(cmp >= 0, d >= e)
            }
        }
    }

}
