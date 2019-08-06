package yuck.core.test

import org.junit._
import scala.collection._

import yuck.core._
import yuck.util.testing.UnitTest

/**
 * @author Michael Marte
 *
 */
@Test
@FixMethodOrder(runners.MethodSorters.NAME_ASCENDING)
final class IntegerDomainTest extends UnitTest {

    private val randomGenerator = new JavaRandomGenerator
    private val helper = new IntegerDomainTestHelper(randomGenerator, logger)
    private val baseRange = new IntegerRange(IntegerValue.get(-5), Five)

    @Test
    def testEquality {
        val CompleteIntegerRangeList = new IntegerRangeList(CompleteIntegerRange)
        assertEq(EmptyIntegerRange.asInstanceOf[IntegerDomain], EmptyIntegerRange)
        assertNe(EmptyIntegerRange.asInstanceOf[IntegerDomain], CompleteIntegerRange)
        assertEq(EmptyIntegerRange, EmptyIntegerRangeList)
        assertNe(EmptyIntegerRange, CompleteIntegerRangeList)
        assertEq(CompleteIntegerRange, CompleteIntegerRangeList)
        assertNe(CompleteIntegerRange, EmptyIntegerRangeList)
        assertEq(new IntegerRange(Zero, Two), new IntegerRangeList(Zero, Two))
        assertNe(new IntegerRange(Zero, Two), new IntegerRangeList(Vector(new IntegerRange(Zero, Zero), new IntegerRange(Two, Two))))
        assertEq(EmptyIntegerRangeList.asInstanceOf[IntegerDomain], EmptyIntegerRange)
        assertNe(EmptyIntegerRangeList.asInstanceOf[IntegerDomain], CompleteIntegerRange)
    }

    @Test
    def testOrdering {
        val sampleSize = 16
        val testData = helper.createTestData(baseRange, sampleSize)
        helper.testOrdering(testData, IntegerDomain.ordering)
    }

    @Test
    def testOperations {
        val sampleSize = 8
        val testData = helper.createTestData(baseRange, sampleSize)
        val extendedBaseRange = new IntegerRange(baseRange.lb - One, baseRange.ub + One)
        helper.testOperations(testData, extendedBaseRange.values.toSeq)
    }

    @Test
    def testRandomSubdomainCreation {
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
    def testDomainCreationFromRanges {
        import IntegerDomain.createDomain
        assert(createDomain(List[IntegerRange]()).isInstanceOf[IntegerRange])
        assert(createDomain(List[IntegerRange]()).isEmpty)
        assert(createDomain(List(CompleteIntegerRange)).isInstanceOf[IntegerRange])
        assert(createDomain(List(CompleteIntegerRange)).isComplete)
        assertEq(
            createDomain(List(NegativeIntegerRange, PositiveIntegerRange)),
            new IntegerRangeList(Vector(NegativeIntegerRange, PositiveIntegerRange)))
    }

    @Test
    def testDomainCreationFromValues {
        import IntegerDomain.createDomain
        val testData = List(
            List(List()) -> EmptyIntegerRange,
            List(List(Zero), List(Zero, Zero)) -> ZeroToZeroIntegerRange,
            List(List(Zero, One), List(One, Zero)) -> ZeroToOneIntegerRange,
            List(List(Zero, One, Three, Four), List(Three, Zero, Four, One, One)) ->
                new IntegerRangeList(Vector(ZeroToOneIntegerRange, new IntegerRange(Three, Four))))
        for ((inputs, expectation) <- testData) {
            for (input <- inputs) {
                def check(values: IterableOnce[IntegerValue]) = {
                    val result = createDomain(values)
                    assertEq(result, expectation)
                    assertEq(result.getClass, expectation.getClass)
                }
                check(input.iterator)
                check(input.toList)
                check(input.toSet)
                check(immutable.TreeSet[IntegerValue]() ++ input)
            }
        }
    }

}
