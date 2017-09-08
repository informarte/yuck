package yuck.core.test

import scala.collection._

import yuck.core._
import yuck.util.testing.YuckAssert

/**
 * @author Michael Marte
 *
 */
class DomainTestHelper[Value <: OrderedValue[Value]] extends YuckAssert {

    // checks that values are chosen uniformly from the given domain
    def testUniformityOfDistribution(rg: RandomGenerator, d: Domain[Value]) {
        val SAMPLE_SIZE = 100000
        val MAX_ERROR = 0.05
        def checkDistribution(f: Map[Value, Int]) {
            for (a <- d.values) {
                assertGt(f.getOrElse(a, 0), SAMPLE_SIZE / d.size * (1 - MAX_ERROR))
                assertLt(f.getOrElse(a, 0), SAMPLE_SIZE / d.size * (1 + MAX_ERROR))
            }
        }
        val f1 = new mutable.HashMap[Value, Int]
        val f2 = new mutable.HashMap[Value, Int]
        for (i <- 1 to SAMPLE_SIZE) {
            val a = d.randomValue(rg)
            assert(d.contains(a))
            f1.put(a, f1.getOrElse(a, 0) + 1)
            val b = d.nextRandomValue(rg, a)
            assert(d.contains(b))
            assertNe(a, b)
            f2.put(b, f2.getOrElse(b, 0) + 1)
        }
        checkDistribution(f1)
        checkDistribution(f2)
    }

}
