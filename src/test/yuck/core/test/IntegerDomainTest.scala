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

    private val helper = new IntegerDomainTestHelper(logger)

    @Test
    def testEquality {
        val CompleteIntegerRangeList = new IntegerRangeList(CompleteIntegerRange)
        assertEq(EmptyIntegerRange.asInstanceOf[IntegerDomain], EmptyIntegerRange)
        assertNe(EmptyIntegerRange.asInstanceOf[IntegerDomain], CompleteIntegerRange)
        assertEq(EmptyIntegerRange, EmptyIntegerRangeList)
        assertEq(CompleteIntegerRange, CompleteIntegerRangeList)
        assertNe(EmptyIntegerRange, CompleteIntegerRangeList)
        assertNe(CompleteIntegerRange, EmptyIntegerRangeList)
        assertEq(new IntegerRange(Zero, Two), new IntegerRangeList(Zero, Two))
        assertNe(new IntegerRange(Zero, Two), new IntegerRangeList(Vector(new IntegerRange(Zero, Zero), new IntegerRange(Two, Two))))
        assertEq(EmptyIntegerRangeList.asInstanceOf[IntegerDomain], EmptyIntegerRange)
        assertNe(EmptyIntegerRangeList.asInstanceOf[IntegerDomain], CompleteIntegerRange)

    }

    @Test
    def testOrdering {
        val SAMPLE_SIZE = 16
        val randomGenerator = new JavaRandomGenerator
        val baseRange = new IntegerRange(Zero, Nine)
        val singletonRanges = baseRange.values.map(a => new IntegerRange(a, a)).toVector
        val randomFiniteRangeLists = for (i <- 1 to SAMPLE_SIZE) yield baseRange.randomSubdomain(randomGenerator)
        val randomInfiniteRangeLists = randomFiniteRangeLists.map(CompleteIntegerRange.diff)
        for ((d, e) <- randomFiniteRangeLists.zip(randomInfiniteRangeLists)) {
            assert(d.union(e).isComplete)
        }
        val edgeCases = List(EmptyIntegerRange, baseRange) ++ helper.specialInfiniteRanges ++ singletonRanges
        val testData = (randomFiniteRangeLists ++ randomInfiniteRangeLists ++ edgeCases ++ edgeCases)
        helper.testOrdering(testData, IntegerDomain.ordering)
    }

    @Test
    def testOperations {
        val SAMPLE_SIZE = 16
        val randomGenerator = new JavaRandomGenerator
        val baseRange = new IntegerRange(Zero, Nine)
        val singletonRanges = baseRange.values.map(a => new IntegerRange(a, a)).toVector
        val randomFiniteRanges = for (i <- 1 to SAMPLE_SIZE) yield baseRange.randomSubrange(randomGenerator)
        val randomFiniteRangeLists = for (i <- 1 to SAMPLE_SIZE) yield baseRange.randomSubdomain(randomGenerator)
        val randomFiniteIntegerDomains = randomFiniteRanges ++ randomFiniteRangeLists
        val randomInfiniteRangeLists = (randomFiniteIntegerDomains).map(CompleteIntegerRange.diff)
        for ((d, e) <- randomFiniteIntegerDomains.zip(randomInfiniteRangeLists)) {
            assert(d.union(e).isComplete)
        }
        val edgeCases = List(EmptyIntegerRange, baseRange) ++ helper.specialInfiniteRanges ++ singletonRanges
        val testData = (randomFiniteIntegerDomains ++ randomInfiniteRangeLists ++ edgeCases).distinct
        helper.testOperations(randomGenerator, testData, (-2 to 11).map(IntegerValue.get))
    }

    @Test
    def testRandomSubdomainCreation {
        val SAMPLE_SIZE = 1000
        val randomGenerator = new JavaRandomGenerator
        val baseRange = new IntegerRange(Zero, Nine)
        val sample = new mutable.HashSet[IntegerDomain]
        for (i <- 1 to SAMPLE_SIZE) {
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
                def check(values: TraversableOnce[IntegerValue]) = {
                    val result = createDomain(values)
                    assertEq(result, expectation)
                    assertEq(result.getClass, expectation.getClass)
                }
                check(input.toIterator)
                check(input.toList)
                check(input.toSet)
                check(immutable.TreeSet[IntegerValue]() ++ input)
            }
        }
    }

}
