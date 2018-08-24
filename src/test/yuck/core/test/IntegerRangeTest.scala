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

    private val randomGenerator = new JavaRandomGenerator
    private val helper = new IntegerDomainTestHelper(randomGenerator, logger)
    private val baseRange = new IntegerRange(IntegerValue.get(-5), Five)

    private def createRange(a: Int, b: Int) = new IntegerRange(IntegerValue.get(a), IntegerValue.get(b))

    @Test
    def testRepresentation {
        helper.testRepresentation((a, b) => new IntegerRange(a, b))
    }

    @Test
    def testEquality {
        val testData =
            List(EmptyIntegerRange) ++ helper.specialInfiniteRanges ++ List(baseRange, createRange(0, 0), createRange(0, 9))
        helper.testEquality(testData)
        for (a <- testData) {
            val b = new IntegerRange(a.lb, a.ub)
            assertEq(a, b)
            assertEq(b, a)
            assertNe(a, False)
            assertNe(False, a)
        }
    }

    @Test
    def testOrdering {
        val SAMPLE_SIZE = 32
        val testData =
            helper.createTestData( baseRange, SAMPLE_SIZE)
                .filter(_.isInstanceOf[IntegerRange]).map(_.asInstanceOf[IntegerRange])
        helper.testOrdering(testData, IntegerRange.ordering)
    }

    @Test
    def testOperations {
        val SAMPLE_SIZE = 16
        val testData =
            helper.createTestData(baseRange, SAMPLE_SIZE)
                .filter(_.isInstanceOf[IntegerRange]).map(_.asInstanceOf[IntegerRange])
        val extendedBaseRange = new IntegerRange(baseRange.lb - One, baseRange.ub + One)
        helper.testOperations(testData, extendedBaseRange.values.toSeq)
    }

    @Test
    def testRandomSubrangeCreation {
        val SAMPLE_SIZE = 1000
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
