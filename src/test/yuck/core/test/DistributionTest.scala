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
class DistributionTest(df: Int => Distribution) extends UnitTest {

    @Test
    def testBasics {
        val n = 256
        val d = df(n)
        var m = 0
        var sum = 0
        assertEq(d.volume, 0)
        assertEq(d.numberOfAlternatives, 0)
        for (i <- 0 until n) {
            val delta = i % 8
            sum += delta
            if (delta > 0) m += 1
            d.addFrequencyDelta(i, delta)
            assertEq(d.frequency(i), delta)
            assertEq(d.cdf(i), sum)
            assertEq(d.cdf(n - 1), sum)
            assertEq(d.volume, sum)
            assertEq(d.numberOfAlternatives, m)
            if (sum > 0) {
                assertEq(d.inverseCdf(sum - 1), if (delta == 0) i - 1 else i)
            }
        }
        for (i <- 0 until n - 1) {
            val delta = i % 8
            sum -= delta
            if (delta > 0) m -= 1
            d.setFrequency(i, 0)
            assertEq(d.frequency(i), 0)
            assertEq(d.cdf(i), 0)
            assertEq(d.cdf(n - 1), sum)
            assertEq(d.volume, sum)
            assertEq(d.numberOfAlternatives, m)
            if (sum > 0) {
                assertEq(d.inverseCdf(0), if ((i + 1) % 8 == 0) i + 2 else i + 1)
                assertEq(d.inverseCdf(sum - 1), n - 1)
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
        val d = df(n)
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
        val d = df(n)
        for (i <- 0 to 2) {
            d.setFrequency(i, i)
        }
        val rg = new JavaRandomGenerator
        val e = df(n)
        for (i <- 0 until 1000) {
            e.addFrequencyDelta(d.nextIndex(rg), 1)
        }
        assertEq(e.frequency(0), 0)
        assertGt(e.frequency(1), 300)
        assertGt(e.frequency(2), 600)
        assertEq(e.volume, 1000)
    }

    @Test
    def testDistributionMaintainer {
        val d = df(3)
        val space = new Space
        val vr = new IntegerDomain(Zero, new IntegerValue(100))
        val x1 = space.createVariable("x1", vr)
        val x2 = space.createVariable("x2", vr)
        val x3 = space.createVariable("x3", vr)
        val axs = immutable.IndexedSeq(new AX(One, x1), new AX(One, x2), new AX(One, x3))
        val c = new DistributionMaintainer(space.constraintIdFactory.nextId, null, axs, d)
        space
            .post(c)
            .setValue(x1, One)
            .setValue(x2, Two)
            .setValue(x3, Three)
            .initialize
        assertEq(d.frequency(0), 1)
        assertEq(d.frequency(1), 2)
        assertEq(d.frequency(2), 3)
        assertEq(d.volume, 6)
        val move = new ChangeValue(space.moveIdFactory.nextId, x1, Five)
        space.consult(move)
        assertEq(d.frequency(0), 1)
        assertEq(d.frequency(1), 2)
        assertEq(d.frequency(2), 3)
        assertEq(d.volume, 6)
        space.commit(move)
        assertEq(d.frequency(0), 5)
        assertEq(d.frequency(1), 2)
        assertEq(d.frequency(2), 3)
        assertEq(d.volume, 10)
    }

}

object DistributionTest {

    @runners.Parameterized.Parameters
    def parameters =
        List(
            Array(n => new ArrayBackedDistribution(n)),
            Array(n => new FenwickTreeBackedDistribution(n))
        ).asJava

}
