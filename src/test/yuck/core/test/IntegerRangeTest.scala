package yuck.core.test

import scala.collection._

import org.junit._

import yuck.core._
import yuck.util.testing.UnitTest

/**
 * @author Michael Marte
 *
 */
@FixMethodOrder(org.junit.runners.MethodSorters.NAME_ASCENDING)
final class IntegerRangeTest extends UnitTest {

    private val helper = new IntegerDomainTestHelper(logger)

    private def createRange(a: Int, b: Int) = new IntegerRange(IntegerValue.get(a), IntegerValue.get(b))

    @Test
    def testBasics {
        val randomGenerator = new JavaRandomGenerator
        helper.testRangeBasics(randomGenerator, (a, b) => new IntegerRange(a, b))
    }

    @Test
    def testOrdering {
        val SAMPLE_SIZE = 32
        val randomGenerator = new JavaRandomGenerator
        val baseRange = new IntegerRange(Zero, Nine)
        val singletonRanges = baseRange.values.map(a => new IntegerRange(a, a)).toVector
        val randomFiniteRanges = for (i <- 1 to SAMPLE_SIZE) yield baseRange.randomSubrange(randomGenerator)
        val edgeCases = List(EmptyIntegerRange, baseRange) ++ helper.specialInfiniteRanges ++ singletonRanges
        val testData = randomFiniteRanges ++ edgeCases ++ edgeCases
        helper.testOrdering(testData, IntegerRange.ordering)
    }

    @Test
    def testRandomSubrangeCreation {
        val SAMPLE_SIZE = 1000
        val randomGenerator = new JavaRandomGenerator
        val baseRange = new IntegerRange(Zero, Nine)
        val sample = new mutable.HashSet[IntegerRange]
        for (i <- 1 to SAMPLE_SIZE) {
            val e = baseRange.randomSubrange(randomGenerator)
            assert(e.isSubsetOf(baseRange))
            sample += e
        }
        assertEq(sample.size, baseRange.size * (baseRange.size + 1) / 2)
    }

    @Test
    def testMultiplication {
        assertEq(createRange(0, 2).mult(createRange(1, 2)), createRange(0, 4))
        assertEq(createRange(0, 2).mult(createRange(-1, 2)), createRange(-2, 4))
        assertEq(createRange(-2, 2).mult(createRange(3, 10)), createRange(-20, 20))
    }

    @Test
    def testDivision {
        assertEq(createRange(-1, 100).div(createRange(-2, 8)), CompleteIntegerRange)
        assertEq(createRange(10, 100).div(createRange(0, 0)), EmptyIntegerRange)
        assertEq(createRange(-100, -10).div(createRange(-2, 5)), createRange(-100, 100))
        assertEq(createRange(1, 100).div(createRange(-7, 0)), createRange(1, 100).div(createRange(-7, -1)))
        assertEq(createRange(1, 100).div(createRange(0, 7)), createRange(1, 100).div(createRange(1, 7)))
        assertEq(createRange(155, 161).div(createRange(9, 11)), createRange(15, 17))
    }

}
