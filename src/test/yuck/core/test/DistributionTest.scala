package yuck.core.test

import org.junit.*

import scala.collection.*
import scala.jdk.CollectionConverters.*

import yuck.core.{given, *}
import yuck.test.util.UnitTest
import yuck.util.arm.scoped

/**
 * @author Michael Marte
 *
 */
@FixMethodOrder(runners.MethodSorters.NAME_ASCENDING)
@runner.RunWith(classOf[runners.Parameterized])
final class DistributionTest(createDistribution: Int => Distribution) extends UnitTest {

    @Test
    def testBasics(): Unit = {
        val N = 256
        val d = createDistribution(N)
        var m = 0
        var volume = 0
        assertEq(d.volume, 0)
        assertEq(d.numberOfAlternatives, 0)
        for (i <- 0 until N) {
            val delta = i % 8
            volume += delta
            if (delta > 0) m += 1
            d.addFrequencyDelta(i, delta)
            assertEq(d.frequency(i), delta)
            assertEq(d.cdf(i), volume)
            assertEq(d.cdf(N - 1), volume)
            assertEq(d.volume, volume)
            assertEq(d.numberOfAlternatives, m)
            if (volume > 0) {
                assertEq(d.probability(i).value, d.frequency(i).toDouble / d.volume.toDouble)
                assertEq(d.inverseCdf(volume - 1), if (delta == 0) i - 1 else i)
            }
        }
        for (i <- 0 until N - 1) {
            val delta = i % 8
            volume -= delta
            if (delta > 0) m -= 1
            d.setFrequency(i, 0)
            assertEq(d.frequency(i), 0)
            assertEq(d.cdf(i), 0)
            assertEq(d.cdf(N - 1), volume)
            assertEq(d.volume, volume)
            assertEq(d.numberOfAlternatives, m)
            if (volume > 0) {
                assertEq(d.probability(i).value, d.frequency(i).toDouble / volume.toDouble)
                assertEq(d.inverseCdf(0), if ((i + 1) % 8 == 0) i + 2 else i + 1)
                assertEq(d.inverseCdf(volume - 1), N - 1)
            }
        }
        assertGt(d.volume, 0L)
        d.clear()
        assertEq(d.volume, 0L)
        assertEq(d.numberOfAlternatives, 0)
    }

    @Test
    def testExceptionalCases(): Unit = {
        val N = 1
        val d = createDistribution(N)
        assertEx(d.setFrequency(-1, 0), classOf[ArrayIndexOutOfBoundsException])
        assertEx(d.setFrequency(N, 0), classOf[ArrayIndexOutOfBoundsException])
        assertEx(d.addFrequencyDelta(-1, 0), classOf[ArrayIndexOutOfBoundsException])
        assertEx(d.addFrequencyDelta(N, 0), classOf[ArrayIndexOutOfBoundsException])
        assertEx(d.frequency(-1), classOf[ArrayIndexOutOfBoundsException])
        assertEx(d.frequency(N), classOf[ArrayIndexOutOfBoundsException])
        assertEx(d.cdf(-1), classOf[ArrayIndexOutOfBoundsException])
        assertEx(d.cdf(N), classOf[ArrayIndexOutOfBoundsException])
        assertEx(d.inverseCdf(-1))
        assertEx(d.inverseCdf(d.volume))
    }

    @Test
    def testRandomIndexGeneration1(): Unit = {
        val N = 3
        val d = createDistribution(N)
        for (i <- 0 until N) {
            d.setFrequency(i, i)
        }
        val randomGenerator = new JavaRandomGenerator
        val result = Array.ofDim[Int](N)
        val SampleSize = 1000
        for (i <- 0 until SampleSize) {
            result(d.nextIndex(randomGenerator)) += 1
        }
        assertEq(result(0), 0)
        assertGt(result(1), 300)
        assertGt(result(2), 600)
        assertEq(result.sum, SampleSize)
    }

    @Test
    def testRandomIndexGeneration2(): Unit = {
        val N = 3
        val d = createDistribution(N)
        for (i <- 0 until N) {
            d.setFrequency(i, i)
        }
        val randomGenerator = new JavaRandomGenerator
        val result = Array.ofDim[Int](N, N)
        val frequencyRestorer = new FrequencyRestorer(N)
        val SampleSize = 1000
        for (i <- 0 until SampleSize) {
            scoped(frequencyRestorer) {
                val choices = d.nextIndices(randomGenerator, N, frequencyRestorer).toArray
                assertEq(choices.size, N - 1)
                assertEq(d.volume, 0)
                for (i <- 0 until choices.size) {
                    result(choices(i))(i) += 1
                }
            }
            for (i <- 0 until N) {
                assertEq(d.frequency(i), i)
            }
        }
        assertEq(result(0).sum, 0)
        assertGt(result(1)(1), 600)
        assertEq(result(1).sum, SampleSize)
        assertGt(result(2)(0), 600)
        assertEq(result(2).sum, SampleSize)
    }

    @Test
    def testOverflowChecking(): Unit = {
        val N = 2
        val d = createDistribution(N)
        d.setFrequency(0, Long.MaxValue)
        assertEx(d.setFrequency(1, 1L), classOf[ArithmeticException])
    }

}

/**
 * @author Michael Marte
 *
 */
object DistributionTest {

    @runners.Parameterized.Parameters
    def parameters =
        List(
            Array(new ArrayBackedDistribution(_)),
            Array(new FenwickTreeBackedDistribution(_))
        ).asJava

}
