package yuck.core.test

import org.junit._
import scala.collection._

import yuck.core._
import yuck.util.testing.UnitTest

/**
 * @author Michael Marte
 *
 */
@FixMethodOrder(runners.MethodSorters.NAME_ASCENDING)
final class IntegerDomainTest extends UnitTest {

    private val randomGenerator = new JavaRandomGenerator
    private val helper = new IntegerDomainTestHelper(randomGenerator, logger)
    private val baseRange = IntegerRange(IntegerValue(-5), Five)

    @Test
    def testEquality: Unit = {
        val CompleteIntegerRangeList = IntegerRangeList(CompleteIntegerRange)
        assertEq(EmptyIntegerRange.asInstanceOf[IntegerDomain], EmptyIntegerRange)
        assertNe(EmptyIntegerRange.asInstanceOf[IntegerDomain], CompleteIntegerRange)
        assertEq(EmptyIntegerRange, EmptyIntegerRangeList)
        assertNe(EmptyIntegerRange, CompleteIntegerRangeList)
        assertEq(CompleteIntegerRange, CompleteIntegerRangeList)
        assertNe(CompleteIntegerRange, EmptyIntegerRangeList)
        assertEq(IntegerRange(Zero, Two), IntegerRangeList(Zero, Two))
        assertNe(IntegerRange(Zero, Two), IntegerRangeList(Vector(IntegerRange(Zero, Zero), IntegerRange(Two, Two))))
        assertEq(EmptyIntegerRangeList.asInstanceOf[IntegerDomain], EmptyIntegerRange)
        assertNe(EmptyIntegerRangeList.asInstanceOf[IntegerDomain], CompleteIntegerRange)
    }

    @Test
    def testOrdering: Unit = {
        val sampleSize = 16
        val testData = helper.createTestData(baseRange, sampleSize)
        helper.testOrdering(testData)
    }

    @Test
    def testOperations: Unit = {
        val sampleSize = 8
        val testData = helper.createTestData(baseRange, sampleSize)
        val extendedBaseRange = IntegerRange(baseRange.lb - One, baseRange.ub + One)
        helper.testOperations(testData, extendedBaseRange.values.toSeq)
    }

    @Test
    def testRandomSubdomainCreation: Unit = {
        val sampleSize = 1000
        val sample = new mutable.HashSet[IntegerDomain]
        for (i <- 1 to sampleSize) {
            val e = baseRange.randomSubdomain(randomGenerator)
            assert(e.isSubsetOf(baseRange))
            sample += e
        }
        assertGt(sample.size, 10 * baseRange.size * (baseRange.size + 1) / 2)
    }

    @Test
    def testDomainCreationFromRanges: Unit = {
        assert(IntegerDomain(List[IntegerRange]()).isInstanceOf[IntegerRange])
        assert(IntegerDomain(List[IntegerRange]()).isEmpty)
        assert(IntegerDomain(List(CompleteIntegerRange)).isInstanceOf[IntegerRange])
        assert(IntegerDomain(List(CompleteIntegerRange)).isComplete)
        assertEq(
            IntegerDomain(List(NegativeIntegerRange, PositiveIntegerRange)),
            IntegerRangeList(Vector(NegativeIntegerRange, PositiveIntegerRange)))
    }

    @Test
    def testDomainCreationFromValues: Unit = {
        val testData = List(
            List(List()) -> EmptyIntegerRange,
            List(List(Zero), List(Zero, Zero)) -> ZeroToZeroIntegerRange,
            List(List(Zero, One), List(One, Zero)) -> ZeroToOneIntegerRange,
            List(List(Zero, One, Three, Four), List(Three, Zero, Four, One, One)) ->
                IntegerRangeList(Vector(ZeroToOneIntegerRange, IntegerRange(Three, Four))))
        for ((inputs, expectation) <- testData) {
            for (input <- inputs) {
                def check(values: Iterable[IntegerValue]) = {
                    val result = IntegerDomain(values)
                    assertEq(result, expectation)
                    assertEq(result.getClass, expectation.getClass)
                }
                check(input.toList)
                check(input.toSet)
                check(immutable.TreeSet[IntegerValue]() ++ input)
            }
        }
    }

}
