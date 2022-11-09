package yuck.core.test

import org.junit.*

import scala.collection.*

import yuck.core.{given, *}
import yuck.test.util.UnitTest

/**
 * @author Michael Marte
 *
 */
@FixMethodOrder(org.junit.runners.MethodSorters.NAME_ASCENDING)
final class SixtyFourBitSetTest extends UnitTest {

    private val randomGenerator = new JavaRandomGenerator
    private val helper = new IntegerDomainTestHelper(randomGenerator, logger)
    private val baseRange = SixtyFourBitSet.ValueRange

    @Test
    def testRepresentation(): Unit = {
        helper.testFiniteRangeRepresentation((a, b) => SixtyFourBitSet(a, b))
        helper.testFiniteRepresentationWithGaps(values => SixtyFourBitSet(IntegerDomain(values)))
    }

    @Test
    def testEquality(): Unit = {
        val sampleSize = 32
        val testData = helper.createBitSets(sampleSize)
        helper.testEquality(testData)
        for (d <- testData) {
            for (e <- List(SixtyFourBitSet(d.set), IntegerDomain(d.values))) {
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
        val sampleSize = 32
        val testData = helper.createBitSets(sampleSize)
        helper.testOrdering(testData)
    }

    @Test
    def testOperations(): Unit = {
        val sampleSize = 16
        val testDomains = helper.createBitSets(sampleSize)
        val testValues = IntegerRange(baseRange.lb - One, baseRange.ub + One).values.toSeq
        helper.testUnaryOperations(testDomains, testValues)
        helper.testBinaryOperations(testDomains)
        for (d <- testDomains) {
            assert(d.intersect(d).isInstanceOf[SixtyFourBitSet])
            assert(d.intersect(baseRange).isInstanceOf[SixtyFourBitSet])
            assert(baseRange.intersect(d).isInstanceOf[SixtyFourBitSet])
            assert(d.union(d).isInstanceOf[SixtyFourBitSet])
            assert(d.union(baseRange).isInstanceOf[SixtyFourBitSet])
            assert(baseRange.union(d).isInstanceOf[SixtyFourBitSet])
            assert(d.diff(d).isInstanceOf[SixtyFourBitSet])
            assert(d.diff(baseRange).isInstanceOf[SixtyFourBitSet])
        }
    }

    @Test
    def testRandomSubdomainCreation(): Unit = {
        val sampleSize = 16
        val testData = helper.createBitSets(sampleSize)
        helper.testRandomSubrangeCreation(testData)
        helper.testRandomSubdomainCreation(testData)
    }

    @Test
    def testConstruction(): Unit = {

        assert(SixtyFourBitSet(0L).eq(EmptyBitSet))
        assert(SixtyFourBitSet(SixtyFourBitSet.MaxUInt).eq(FullBitSet))
        assertEq(SixtyFourBitSet(13L).set, 13L)

        assertEx(SixtyFourBitSet(-1, 0))
        assertEx(SixtyFourBitSet(0, 64))
        assert(SixtyFourBitSet(1, 0).eq(EmptyBitSet))
        assert(SixtyFourBitSet(0, 63).eq(FullBitSet))
        assertEq(SixtyFourBitSet(0, 1).set, 3L)

        assert(SixtyFourBitSet(List[Long]()).eq(EmptyBitSet))
        assert(SixtyFourBitSet(0L to 63L).eq(FullBitSet))
        assertEx(SixtyFourBitSet(List(-1L)).set)
        assertEx(SixtyFourBitSet(List(64L)).set)
        assertEq(SixtyFourBitSet(List(0L, 63L)).set, 0x8000000000000001L)

        assertEx(SixtyFourBitSet(MinusOne, Zero))
        assertEx(SixtyFourBitSet(Zero, IntegerValue(64)))
        assert(SixtyFourBitSet(One, Zero).eq(EmptyBitSet))
        assert(SixtyFourBitSet(Zero, IntegerValue(63)).eq(FullBitSet))
        assertEq(SixtyFourBitSet(Zero, One).set, 3L)

        assert(SixtyFourBitSet(EmptyBitSet).eq(EmptyBitSet))
        assert(SixtyFourBitSet(EmptyIntegerRange).eq(EmptyBitSet))
        assert(SixtyFourBitSet(EmptyIntegerRangeList).eq(EmptyBitSet))
        assert(SixtyFourBitSet(IntegerRange(0, 63)).eq(FullBitSet))
        assertEx(SixtyFourBitSet(IntegerDomain(-1)))
        assertEx(SixtyFourBitSet(IntegerDomain(64)))
        assertEq(SixtyFourBitSet(IntegerDomain(0, 63)).set, 0x8000000000000001L)

    }

}
