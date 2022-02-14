package yuck.core.test

import org.junit.*

import scala.collection.*

import yuck.core.*
import yuck.test.util.UnitTest

/**
 * @author Michael Marte
 *
 */
@FixMethodOrder(runners.MethodSorters.NAME_ASCENDING)
final class RandomGeneratorTest extends UnitTest {

    private def testShuffling(shuffle: (RandomGenerator, IndexedSeq[Int]) => IndexedSeq[Int]): Unit = {
        val n = 1000
        val m = 10
        var data: IndexedSeq[Int] = mutable.ArrayBuffer.tabulate(m)(identity)
        val randomGenerator = new JavaRandomGenerator
        val numberOfChangedPositionsDistribution = new Array[Int](m + 1)
        for (i <- 1 to n) {
            val shuffledData = shuffle(randomGenerator, data)
            var numberOfChangedPositions = 0
            for (j <- 0 until m) {
                if (shuffledData(j) != data(j)) numberOfChangedPositions += 1
            }
            numberOfChangedPositionsDistribution(numberOfChangedPositions) += 1
            data = shuffledData
        }
        assertEq(numberOfChangedPositionsDistribution(0), 0)
        assertGt(numberOfChangedPositionsDistribution(6), 10)
        assertGt(numberOfChangedPositionsDistribution(7), 50)
        assertGt(numberOfChangedPositionsDistribution(8), 150)
        assertGt(numberOfChangedPositionsDistribution(9), 350)
        assertGt(numberOfChangedPositionsDistribution(10), 350)
    }

    @Test
    def testEagerShuffling: Unit = {
        testShuffling(
            (randomGenerator, data) => randomGenerator.shuffle(data))
    }

    @Test
    def testLazyShuffling: Unit = {
        testShuffling(
            (randomGenerator, data) => {
                val shuffledData = new mutable.ArrayBuffer[Int]
                shuffledData ++= randomGenerator.lazyShuffle(data)
                shuffledData
            })
    }

}
