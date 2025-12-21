package yuck.core.test

import scala.collection.Seq

import yuck.core.*
import yuck.core.IntegerDomain.ensureRangeList

trait IntegerDomainTestDataFactory {

    import IntegerDomainTestDataFactory.SpecialInfiniteRanges

    protected val randomGenerator: RandomGenerator

    def createRanges(baseRange: IntegerRange, sampleSize: Int): Seq[IntegerRange] = {
        require(baseRange.isFinite)
        val singletonRanges = List(baseRange.lb, baseRange.ub).map(a => IntegerRange(a, a))
        val randomFiniteRanges = for i <- 1 to sampleSize yield baseRange.randomSubrange(randomGenerator)
        val ranges =
            List(SpecialInfiniteRanges, List(EmptyIntegerRange, baseRange), singletonRanges, randomFiniteRanges)
                .flatten.distinct
        ranges
    }

    def createRangeLists(baseRange: IntegerRange, sampleSize: Int): Seq[IntegerRangeList] = {
        val ranges = createRanges(baseRange, sampleSize)
        val randomFiniteRanges = ranges.filter(_.isFinite)
        val randomFiniteRangeLists = for i <- 1 to sampleSize yield ensureRangeList(baseRange.randomSubdomain(randomGenerator))
        val randomFiniteIntegerDomains = randomFiniteRanges ++ randomFiniteRangeLists
        val randomInfiniteRangeLists =
            for infiniteRange <- SpecialInfiniteRanges;
                 finiteDomain <- randomFiniteIntegerDomains;
                 if infiniteRange.intersects(finiteDomain) yield ensureRangeList(infiniteRange.diff(finiteDomain))
        val rangeLists = List(ranges.map(ensureRangeList), randomFiniteRangeLists, randomFiniteRangeLists).flatten.distinct
        rangeLists
    }

    def createBitSets(sampleSize: Int): Seq[SixtyFourBitSet] = {
        val singletonBitSets = List(SixtyFourBitSet.ValueRange.lb, SixtyFourBitSet.ValueRange.ub).map(a => SixtyFourBitSet(a, a))
        val randomBitSets = for i <- 1 to sampleSize yield FullBitSet.randomSubdomain(randomGenerator)
        val bitSets = List(List(EmptyBitSet, FullBitSet), singletonBitSets, randomBitSets).flatten.distinct
        bitSets
    }

    def createTestData(baseRange: IntegerRange, sampleSize: Int): Seq[IntegerDomain] =
        createRanges(baseRange, sampleSize) ++
        createRangeLists(baseRange, sampleSize) ++
        createBitSets(sampleSize)

}

object IntegerDomainTestDataFactory {

    private val SpecialInfiniteRanges = List(
        CompleteIntegerRange,
        NegativeIntegerRange, NonNegativeIntegerRange,
        PositiveIntegerRange, NonPositiveIntegerRange)

}
