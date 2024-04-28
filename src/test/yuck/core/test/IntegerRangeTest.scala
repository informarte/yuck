package yuck.core.test

import org.junit.*

import scala.collection.*

import yuck.core.{given, *}
import yuck.test.*
import yuck.test.util.UnitTest

/**
 * @author Michael Marte
 *
 */
@FixMethodOrder(org.junit.runners.MethodSorters.NAME_ASCENDING)
final class IntegerRangeTest extends UnitTest {

    private val BaseRange = IntegerRange(-5, 5)

    private val randomGenerator = new JavaRandomGenerator
    private val helper = new IntegerDomainTestHelper(randomGenerator, logger)

    @Test
    def testRepresentation(): Unit = {
        helper.testRangeRepresentation((a, b) => IntegerRange(a, b))
    }

    @Test
    def testEquality(): Unit = {
        val testData = helper.createRanges(BaseRange, 32)
        helper.testEquality(testData)
        for (d <- testData) {
            for (e <- List(IntegerRange(d.lb, d.ub), IntegerRangeList(d))) {
                assertEq(d, e)
                assertEq(e, d)
                assertNe(d, False)
                assertNe(False, d)
                for (e <- testData) {
                    assert(if (d.eq(e)) d == e else d != e)
                }
            }
        }
    }

    @Test
    def testOrdering(): Unit = {
        val testData = helper.createRanges(BaseRange, 32)
        helper.testOrdering(testData)
    }

    @Test
    def testOperations(): Unit = {
        val testDomains = helper.createRanges(BaseRange, 16)
        val testValues = IntegerRange(BaseRange.lb - One, BaseRange.ub + One).values.toSeq
        helper.testUnaryOperations(testDomains, testValues)
        helper.testBinaryOperations(testDomains)
    }

    @Test
    def testRandomSubdomainCreation(): Unit = {
        val testData = helper.createRanges(BaseRange, 16)
        helper.testRandomSubrangeCreation(testData)
        helper.testRandomSubdomainCreation(testData)
    }

    @Test
    def testMultiplication(): Unit = {
        assertEq(EmptyIntegerRange.mult(EmptyIntegerRange), EmptyIntegerRange)
        assertEq(EmptyIntegerRange.mult(IntegerRange(1, 10)), EmptyIntegerRange)
        assertEq(IntegerRange(1, 10).mult(EmptyIntegerRange), EmptyIntegerRange)
        assertEq(IntegerRange(0, 2).mult(IntegerRange(1, 2)), IntegerRange(0, 4))
        assertEq(IntegerRange(0, 2).mult(IntegerRange(-1, 2)), IntegerRange(-2, 4))
        assertEq(IntegerRange(-2, 2).mult(IntegerRange(3, 10)), IntegerRange(-20, 20))
    }

    @Test
    def testDivision(): Unit = {
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

    @Test
    def testConstruction(): Unit = {
        assert(IntegerRange(null, null).eq(CompleteIntegerRange))
        assert(IntegerRange(One, Zero).eq(EmptyIntegerRange))
        assert(IntegerRange(1, 0).eq(EmptyIntegerRange))
        assert(IntegerRange(0, 1).isInstanceOf[IntegerRange])
        assertEq(IntegerRange(0, 1), IntegerRange(Zero, One))
    }

}
