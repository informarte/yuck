package yuck.core.test

import scala.collection.Seq

import yuck.core.*
import yuck.test.util.OrderingTestHelper
import yuck.util.OrderingFromOrdered
import yuck.util.logging.LazyLogger
import yuck.util.logging.LogLevel.FineLogLevel

/**
 * @author Michael Marte
 *
 */
abstract class OrderedDomainTestHelper[V <: OrderedValue[V]] extends DomainTestHelper[V] {

    protected val randomGenerator: RandomGenerator

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
