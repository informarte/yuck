package yuck.core.test.util

import scala.collection.{Map, mutable}

import yuck.core.{Domain, RandomGenerator, Value}
import yuck.test.util.YuckAssert

trait RandomValueSelectionTestTooling[A <: Value[A], D <: Domain[A, D]] extends YuckAssert {

    protected val randomGenerator: RandomGenerator

    // Checks that values are chosen uniformly from the given domain.
    def testUniformityOfDistribution(d: D): Unit = {
        val sampleSize = 100000
        val maxError = 0.05
        def checkDistribution(f: Map[A, Int]): Unit = {
            for a <- d.values do {
                import scala.math.Ordering.Double.TotalOrdering
                assertGt(f.getOrElse(a, 0).toDouble, sampleSize / d.size * (1 - maxError))
                assertLt(f.getOrElse(a, 0).toDouble, sampleSize / d.size * (1 + maxError))
            }
        }
        val f1 = new mutable.HashMap[A, Int]
        val f2 = new mutable.HashMap[A, Int]
        for i <- 1 to sampleSize do {
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
