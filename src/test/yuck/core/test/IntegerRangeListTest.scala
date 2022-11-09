package yuck.core.test

import org.junit.*

import scala.collection.Seq

import yuck.core.{given, *}
import yuck.core.IntegerDomain.ensureRangeList
import yuck.test.util.UnitTest

/**
 * @author Michael Marte
 *
 */
@FixMethodOrder(runners.MethodSorters.NAME_ASCENDING)
final class IntegerRangeListTest extends UnitTest {

    private val randomGenerator = new JavaRandomGenerator
    private val helper = new IntegerDomainTestHelper(randomGenerator, logger)
    private val baseRange = IntegerRange(IntegerValue(-5), Five)

    @Test
    def testRepresentation(): Unit = {

        helper.testRangeRepresentation((a, b) => IntegerRangeList(a, b))
        helper.testFiniteRepresentationWithGaps(values => ensureRangeList(IntegerDomain(values)))

        // ]-inf, +inf[ \ ([0, 9] \ {5, 7})
        val d = IntegerRangeList(Vector(IntegerRange(null, MinusOne), IntegerRange(Five, Five), IntegerRange(Seven, Seven), IntegerRange(Ten, null)))
        assertEq(d.toString, "-inf..-1 ∪ {5} ∪ {7} ∪ 10..+inf")
        assert(! d.isEmpty)
        assertEx(d.size)
        assert(! d.isComplete)
        assert(! d.isFinite)
        assert(! d.isSingleton)
        assert(d.contains(Five))
        assertEx(d.singleValue)
        assertEx(d.values)
        assertEx(d.valuesIterator)
        assert(! d.isBounded)
        assert(! d.hasLb)
        assert(! d.hasUb)
        assert(d.maybeLb.isEmpty)
        assert(d.maybeUb.isEmpty)
        assertEq(d.lb, null)
        assertEq(d.ub, null)
        assert(d.hasGaps)

    }

    @Test
    def testEquality(): Unit = {
        val sampleSize = 32
        val testData = helper.createRangeLists(baseRange, sampleSize)
        helper.testEquality(testData)
        for (d <- testData) {
            val e = IntegerRangeList(d.ranges)
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
    def testOrdering(): Unit = {
        val sampleSize = 32
        val testData = helper.createRangeLists(baseRange, sampleSize)
        helper.testOrdering(testData)
    }

    @Test
    def testOperations(): Unit = {
        val sampleSize = 16
        val testDomains = helper.createRangeLists(baseRange, sampleSize)
        val testValues = IntegerRange(baseRange.lb - One, baseRange.ub + One).values.toSeq
        helper.testUnaryOperations(testDomains, testValues)
        helper.testBinaryOperations(testDomains)
    }

    @Test
    def testRandomSubdomainCreation(): Unit = {
        val sampleSize = 16
        val testData = helper.createRangeLists(baseRange, sampleSize)
        helper.testRandomSubrangeCreation(testData)
        helper.testRandomSubdomainCreation(testData)
    }

}
