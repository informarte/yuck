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
        val sampleSize = 32
        val testData =
            helper.createTestData(baseRange, sampleSize)
                .filter(_.isInstanceOf[IntegerRange]).map(_.asInstanceOf[IntegerRange])
        helper.testEquality(testData)
        for (d <- testData) {
            val e = IntegerRange(d.lb, d.ub)
            assertEq(d, e)
            assertEq(e, d)
            assertNe(d, False)
            assertNe(False, d)
            for (e <- testData) {
                assert(if (d.eq(e)) d == e else d != e)
            }
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
    def testRandomSubdomainCreation: Unit = {
        val testData = IntegerDomainTestHelper.createEdgeCases(baseRange)
        helper.testRandomSubrangeCreation(testData)
        helper.testRandomSubdomainCreation(testData)
    }

    @Test
    def testMultiplication: Unit = {
        assertEq(EmptyIntegerRange.mult(EmptyIntegerRange), EmptyIntegerRange)
        assertEq(EmptyIntegerRange.mult(IntegerRange(1, 10)), EmptyIntegerRange)
        assertEq(IntegerRange(1, 10).mult(EmptyIntegerRange), EmptyIntegerRange)
        assertEq(IntegerRange(0, 2).mult(IntegerRange(1, 2)), IntegerRange(0, 4))
        assertEq(IntegerRange(0, 2).mult(IntegerRange(-1, 2)), IntegerRange(-2, 4))
        assertEq(IntegerRange(-2, 2).mult(IntegerRange(3, 10)), IntegerRange(-20, 20))
    }

    @Test
    def testDivision: Unit = {
        assertEq(EmptyIntegerRange.div(EmptyIntegerRange), EmptyIntegerRange)
        assertEq(EmptyIntegerRange.div(IntegerRange(1, 10)), EmptyIntegerRange)
        assertEq(IntegerRange(1, 10).div(EmptyIntegerRange), EmptyIntegerRange)
        assertEq(IntegerRange(-1, 100).div(IntegerRange(-2, 8)), CompleteIntegerRange)
        assertEq(IntegerRange(10, 100).div(IntegerRange(0, 0)), EmptyIntegerRange)
        assertEq(IntegerRange(-100, -10).div(IntegerRange(-2, 5)), IntegerRange(-100, 100))
        assertEq(IntegerRange(1, 100).div(IntegerRange(-7, 0)), IntegerRange(1, 100).div(IntegerRange(-7, -1)))
        assertEq(IntegerRange(1, 100).div(IntegerRange(0, 7)), IntegerRange(1, 100).div(IntegerRange(1, 7)))
        assertEq(IntegerRange(155, 161).div(IntegerRange(9, 11)), IntegerRange(15, 17))
    }

}
