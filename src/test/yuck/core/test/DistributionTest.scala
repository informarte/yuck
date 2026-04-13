package yuck.core.test

import org.junit.jupiter.api.Test
import org.junit.jupiter.params.ParameterizedClass
import org.junit.jupiter.params.provider.MethodSource

import yuck.core.*
import yuck.test.util.UnitTest
import yuck.util.arm.scoped

@ParameterizedClass
@MethodSource(Array("parameters"))
final class DistributionTest(createDistribution: Int => Distribution) extends UnitTest {

    @Test
    def testBasics(): Unit = {
        val n = 256
        val d = createDistribution(n)
        var m = 0
        var volume = 0
        assertEq(d.volume, 0)
        assertEq(d.numberOfAlternatives, 0)
        for i <- 0 until n do {
            val delta = i % 8
            volume += delta
            if delta > 0 then {
                m += 1
            }
            d.addFrequencyDelta(i, delta)
            assertEq(d.frequency(i), delta)
            assertEq(d.cdf(i), volume)
            assertEq(d.cdf(n - 1), volume)
            assertEq(d.volume, volume)
            assertEq(d.numberOfAlternatives, m)
            if volume > 0 then {
                assertEq(d.probability(i).value, d.frequency(i).toDouble / d.volume.toDouble)
                assertEq(d.inverseCdf(volume - 1), if delta == 0 then i - 1 else i)
            }
        }
        for i <- 0 until n - 1 do {
            val delta = i % 8
            volume -= delta
            if delta > 0 then {
                m -= 1
            }
            d.setFrequency(i, 0)
            assertEq(d.frequency(i), 0)
            assertEq(d.cdf(i), 0)
            assertEq(d.cdf(n - 1), volume)
            assertEq(d.volume, volume)
            assertEq(d.numberOfAlternatives, m)
            if volume > 0 then {
                assertEq(d.probability(i).value, d.frequency(i).toDouble / volume.toDouble)
                assertEq(d.inverseCdf(0), if (i + 1) % 8 == 0 then i + 2 else i + 1)
                assertEq(d.inverseCdf(volume - 1), n - 1)
            }
        }
        assertGt(d.volume, 0L)
        d.clear()
        assertEq(d.volume, 0L)
        assertEq(d.numberOfAlternatives, 0)
    }

    @Test
    def testExceptionalCases(): Unit = {
        val n = 1
        val d = createDistribution(n)
        assertThrows(d.setFrequency(-1, 0), classOf[ArrayIndexOutOfBoundsException])
        assertThrows(d.setFrequency(n, 0), classOf[ArrayIndexOutOfBoundsException])
        assertThrows(d.addFrequencyDelta(-1, 0), classOf[ArrayIndexOutOfBoundsException])
        assertThrows(d.addFrequencyDelta(n, 0), classOf[ArrayIndexOutOfBoundsException])
        assertThrows(d.frequency(-1), classOf[ArrayIndexOutOfBoundsException])
        assertThrows(d.frequency(n), classOf[ArrayIndexOutOfBoundsException])
        assertThrows(d.cdf(-1), classOf[ArrayIndexOutOfBoundsException])
        assertThrows(d.cdf(n), classOf[ArrayIndexOutOfBoundsException])
        assertThrows(d.inverseCdf(-1))
        assertThrows(d.inverseCdf(d.volume))
    }

    @Test
    def testRandomIndexGeneration1(): Unit = {
        val n = 3
        val d = createDistribution(n)
        for i <- 0 until n do {
            d.setFrequency(i, i)
        }
        val randomGenerator = new JavaRandomGenerator
        val result = Array.ofDim[Int](n)
        val sampleSize = 1000
        for i <- 0 until sampleSize do {
            result(d.nextIndex(randomGenerator)) += 1
        }
        assertEq(result(0), 0)
        assertGt(result(1), 300)
        assertGt(result(2), 600)
        assertEq(result.sum, sampleSize)
    }

    @Test
    def testRandomIndexGeneration2(): Unit = {
        val n = 3
        val d = createDistribution(n)
        for i <- 0 until n do {
            d.setFrequency(i, i)
        }
        val randomGenerator = new JavaRandomGenerator
        val result = Array.ofDim[Int](n, n)
        val frequencyRestorer = new FrequencyRestorer(n)
        val sampleSize = 1000
        for i <- 0 until sampleSize do {
            scoped(frequencyRestorer) {
                val choices = d.nextIndices(randomGenerator, n, frequencyRestorer).toArray
                assertEq(choices.size, n - 1)
                assertEq(d.volume, 0)
                for i <- 0 until choices.size do {
                    result(choices(i))(i) += 1
                }
            }
            for i <- 0 until n do {
                assertEq(d.frequency(i), i)
            }
        }
        assertEq(result(0).sum, 0)
        assertGt(result(1)(1), 600)
        assertEq(result(1).sum, sampleSize)
        assertGt(result(2)(0), 600)
        assertEq(result(2).sum, sampleSize)
    }

    @Test
    def testOverflowChecking(): Unit = {
        val n = 2
        val d = createDistribution(n)
        d.setFrequency(0, Long.MaxValue)
        assertThrows(d.setFrequency(1, 1L), classOf[ArithmeticException])
    }

}

object DistributionTest {

    def parameters =
        Array[AnyRef](
            Array(new ArrayBackedDistribution(_)),
            Array(new FenwickTreeBackedDistribution(_))
        )

}
