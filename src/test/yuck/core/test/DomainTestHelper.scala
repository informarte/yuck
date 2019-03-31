package yuck.core.test

import scala.collection._

import yuck.core._
import yuck.util.logging.LazyLogger
import yuck.util.testing.{EqualityTestHelper, YuckAssert}

/**
 * @author Michael Marte
 *
 */
class DomainTestHelper[Value <: AnyValue](logger: LazyLogger) extends YuckAssert {

    def testEquality(testData: Seq[Domain[Value]]) {
        logger.withLogScope("Test data") {
            testData.foreach(item => logger.log(item.toString))
        }
        val helper = new EqualityTestHelper[Domain[Value]]
        helper.testEquality(testData)
    }

    // Checks that values are chosen uniformly from the given domain.
    def testUniformityOfDistribution(randomGenerator: RandomGenerator, d: Domain[Value]) {
        val sampleSize = 100000
        val maxError = 0.05
        def checkDistribution(f: Map[Value, Int]) {
            for (a <- d.values) {
                assertGt(f.getOrElse(a, 0), sampleSize / d.size * (1 - maxError))
                assertLt(f.getOrElse(a, 0), sampleSize / d.size * (1 + maxError))
            }
        }
        val f1 = new mutable.AnyRefMap[Value, Int]
        val f2 = new mutable.AnyRefMap[Value, Int]
        for (i <- 1 to sampleSize) {
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
