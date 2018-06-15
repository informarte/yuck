package yuck.core.test

import scala.collection._
import scala.collection.JavaConverters._

import org.junit._

import yuck.constraints.DistributionMaintainer
import yuck.core._
import yuck.util.testing.UnitTest

/**
 * @author Michael Marte
 *
 */
@Test
@FixMethodOrder(runners.MethodSorters.NAME_ASCENDING)
@runner.RunWith(classOf[runners.Parameterized])
final class DistributionTest(createDistribution: Int => Distribution) extends UnitTest {

    @Test
    def testBasics {
        val n = 256
        val d = createDistribution(n)
        var m = 0
        var volume = 0
        assertEq(d.volume, 0)
        assertEq(d.numberOfAlternatives, 0)
        for (i <- 0 until n) {
            val delta = i % 8
            volume += delta
            if (delta > 0) m += 1
            d.addFrequencyDelta(i, delta)
            assertEq(d.frequency(i), delta)
            assertEq(d.cdf(i), volume)
            assertEq(d.cdf(n - 1), volume)
            assertEq(d.volume, volume)
            assertEq(d.numberOfAlternatives, m)
            if (volume > 0) {
                assertEq(d.probability(i).value, d.frequency(i).toDouble / d.volume.toDouble)
                assertEq(d.inverseCdf(volume - 1), if (delta == 0) i - 1 else i)
            }
        }
        for (i <- 0 until n - 1) {
            val delta = i % 8
            volume -= delta
            if (delta > 0) m -= 1
            d.setFrequency(i, 0)
            assertEq(d.frequency(i), 0)
            assertEq(d.cdf(i), 0)
            assertEq(d.cdf(n - 1), volume)
            assertEq(d.volume, volume)
            assertEq(d.numberOfAlternatives, m)
            if (volume > 0) {
                assertEq(d.probability(i).value, d.frequency(i).toDouble / volume.toDouble)
                assertEq(d.inverseCdf(0), if ((i + 1) % 8 == 0) i + 2 else i + 1)
                assertEq(d.inverseCdf(volume - 1), n - 1)
            }
        }
        assertGt(d.volume, 0)
        d.clear
        assertEq(d.volume, 0)
        assertEq(d.numberOfAlternatives, 0)
    }

    @Test
    def testExceptionalCases {
        val n = 1
        val d = createDistribution(n)
        assertEx(d.setFrequency(-1, 0))
        assertEx(d.setFrequency(n, 0))
        assertEx(d.addFrequencyDelta(-1, 0))
        assertEx(d.addFrequencyDelta(n, 0))
        assertEx(d.frequency(-1))
        assertEx(d.frequency(n))
        assertEx(d.cdf(-1))
        assertEx(d.cdf(n))
        assertEx(d.inverseCdf(-1))
        assertEx(d.inverseCdf(d.volume))
    }

    @Test
    def testRandomIndexGeneration {
        val n = 3
        val d = createDistribution(n)
        for (i <- 0 to 2) {
            d.setFrequency(i, i)
        }
        val randomGenerator = new JavaRandomGenerator
        val e = createDistribution(n)
        val SAMPLE_SIZE = 1000
        for (i <- 0 until SAMPLE_SIZE) {
            e.addFrequencyDelta(d.nextIndex(randomGenerator), 1)
        }
        assertEq(e.frequency(0), 0)
        assertGt(e.frequency(1), 330)
        assertGt(e.frequency(2), 650)
        assertEq(e.volume, 1000)
    }

    @Test
    def testOverflowChecking {
        val n = 2
        val d = createDistribution(n)
        d.setFrequency(0, Int.MaxValue)
        assertEx(d.setFrequency(1, Int.MaxValue), classOf[ArithmeticException])
    }

}

/**
 * @author Michael Marte
 *
 */
final object DistributionTest {

    @runners.Parameterized.Parameters
    def parameters =
        List(
            Array(new ArrayBackedDistribution(_)),
            Array(new FenwickTreeBackedDistribution(_))
        ).asJava

}
