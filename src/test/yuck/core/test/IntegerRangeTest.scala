package yuck.core.test

import scala.collection._
import org.junit._

import yuck.core._
import yuck.test.util.UnitTest

/**
 * @author Michael Marte
 *
 */
@FixMethodOrder(org.junit.runners.MethodSorters.NAME_ASCENDING)
final class IntegerRangeTest extends UnitTest {

    private val randomGenerator = new JavaRandomGenerator
    private val helper = new IntegerDomainTestHelper(randomGenerator, logger)
    private val baseRange = IntegerRange(IntegerValue(-5), Five)

    private def createRange(a: Int, b: Int) = IntegerRange(IntegerValue(a), IntegerValue(b))

    @Test
    def testRepresentation: Unit = {
        helper.testRepresentation((a, b) => IntegerRange(a, b))
    }

    @Test
    def testConstructionFromBoundaries: Unit = {
        assert(IntegerRange(null, null).eq(CompleteIntegerRange))
        assert(IntegerRange(One, Zero).eq(EmptyIntegerRange))
        assertEq(IntegerRange(Zero, One), ZeroToOneIntegerRange)
        assert(IntegerRange(1, 0).eq(EmptyIntegerRange))
        assertEq(IntegerRange(0, 1), ZeroToOneIntegerRange)
    }

    @Test
    def testEquality: Unit = {
        val testData =
            List(EmptyIntegerRange) ++ helper.specialInfiniteRanges ++ List(baseRange, createRange(0, 0), createRange(0, 9))
        helper.testEquality(testData)
        for (a <- testData) {
            val b = IntegerRange(a.lb, a.ub)
            assertEq(a, b)
            assertEq(b, a)
            assertNe(a, False)
            assertNe(False, a)
        }
    }

    @Test
    def testOrdering: Unit = {
        val sampleSize = 32
        val testData =
            helper.createTestData(baseRange, sampleSize)
                .filter(_.isInstanceOf[IntegerRange]).map(_.asInstanceOf[IntegerRange])
        helper.testOrdering(testData)
    }

    @Test
    def testOperations: Unit = {
        val sampleSize = 16
        val testData =
            helper.createTestData(baseRange, sampleSize)
                .filter(_.isInstanceOf[IntegerRange]).map(_.asInstanceOf[IntegerRange])
        val extendedBaseRange = IntegerRange(baseRange.lb - One, baseRange.ub + One)
        helper.testOperations(testData, extendedBaseRange.values.toSeq)
    }

    @Test
    def testRandomSubrangeCreation: Unit = {
        val sampleSize = 1000
        val sample = new mutable.HashSet[IntegerRange]
        for (i <- 1 to sampleSize) {
            val e = baseRange.randomSubrange(randomGenerator)
            assert(e.isSubsetOf(baseRange))
            sample += e
        }
        assertEq(sample.size, baseRange.size * (baseRange.size + 1) / 2)
    }

    @Test
    def testMultiplication: Unit = {
        assertEq(EmptyIntegerRange.mult(EmptyIntegerRange), EmptyIntegerRange)
        assertEq(EmptyIntegerRange.mult(createRange(1, 10)), EmptyIntegerRange)
        assertEq(createRange(1, 10).mult(EmptyIntegerRange), EmptyIntegerRange)
        assertEq(createRange(0, 2).mult(createRange(1, 2)), createRange(0, 4))
        assertEq(createRange(0, 2).mult(createRange(-1, 2)), createRange(-2, 4))
        assertEq(createRange(-2, 2).mult(createRange(3, 10)), createRange(-20, 20))
    }

    @Test
    def testDivision: Unit = {
        assertEq(EmptyIntegerRange.div(EmptyIntegerRange), EmptyIntegerRange)
        assertEq(EmptyIntegerRange.div(createRange(1, 10)), EmptyIntegerRange)
        assertEq(createRange(1, 10).div(EmptyIntegerRange), EmptyIntegerRange)
        assertEq(createRange(-1, 100).div(createRange(-2, 8)), CompleteIntegerRange)
        assertEq(createRange(10, 100).div(createRange(0, 0)), EmptyIntegerRange)
        assertEq(createRange(-100, -10).div(createRange(-2, 5)), createRange(-100, 100))
        assertEq(createRange(1, 100).div(createRange(-7, 0)), createRange(1, 100).div(createRange(-7, -1)))
        assertEq(createRange(1, 100).div(createRange(0, 7)), createRange(1, 100).div(createRange(1, 7)))
        assertEq(createRange(155, 161).div(createRange(9, 11)), createRange(15, 17))
    }

}
