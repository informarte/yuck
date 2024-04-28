package yuck.core.test

import scala.collection.*

import yuck.core.{given, *}
import yuck.test.util.{EqualityTestHelper, YuckAssert}
import yuck.util.logging.{FineLogLevel, LazyLogger}

/**
 * @author Michael Marte
 *
 */
abstract class DomainTestHelper[V <: Value[V]] extends YuckAssert {

    protected val logger: LazyLogger

    def testEquality(testData: Seq[Domain[V]]): Unit = {
        logger.withRootLogLevel(FineLogLevel) {
            logger.withLogScope("Test data") {
                testData.foreach(item => logger.log(item.toString))
            }
        }
        val helper = new EqualityTestHelper[Domain[V]]
        helper.testEquality(testData)
        for (d <- testData) {
            for (e <- testData) {
                assert(if (d.eq(e)) d == e else d != e)
            }
        }
    }

    // Checks that values are chosen uniformly from the given domain.
    def testUniformityOfDistribution(randomGenerator: RandomGenerator, d: Domain[V]): Unit = {
        val SampleSize = 100000
        val MaxError = 0.05
        def checkDistribution(f: Map[V, Int]): Unit = {
            for (a <- d.values) {
                import scala.math.Ordering.Double.TotalOrdering
                assertGt(f.getOrElse(a, 0).toDouble, SampleSize / d.size * (1 - MaxError))
                assertLt(f.getOrElse(a, 0).toDouble, SampleSize / d.size * (1 + MaxError))
            }
        }
        val f1 = new mutable.AnyRefMap[V, Int]
        val f2 = new mutable.AnyRefMap[V, Int]
        for (i <- 1 to SampleSize) {
            val a = d.randomValue(randomGenerator)
            assert(d.contains(a))
            f1.put(a, f1.getOrElse(a, 0) + 1)
            val b = d.nextRandomValue(randomGenerator, a)
            assert(d.contains(b))
            assertNe(a, b)
            f2.put(b, f2.getOrElse(b, 0) + 1)
        }
        checkDistribution(f1)
        checkDistribution(f2)
    }

}
