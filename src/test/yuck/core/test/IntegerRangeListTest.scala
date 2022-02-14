package yuck.core.test

import org.junit.*

import yuck.core.*
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
    private val testData = List(
        IntegerRangeList(Vector(IntegerRange(Zero, Four), IntegerRange(Six, Nine))),
        IntegerRangeList(Vector(IntegerRange(Zero, Four), IntegerRange(Six, Six), IntegerRange(Eight, Nine))),
        IntegerRangeList(Vector(IntegerRange(Zero, Four), IntegerRange(Eight, Nine))),
        IntegerRangeList(Vector(IntegerRange(null, MinusOne), IntegerRange(Five, Five), IntegerRange(Seven, Seven), IntegerRange(Ten, null))))

    @Test
    def testRepresentation: Unit = {

        helper.testRepresentation((a, b) => IntegerRangeList(a, b))
        val List(a, b, c, d) = testData

        // [0, 9] \ {5}
        assert(! a.isEmpty)
        assertEq(a.size, 9)
        assertEq(a.toString, "0..4 union 6..9")
        assert(! a.contains(Five))
        assertEq(a.values.size, 9)
        assertEq(a.values.toList, List(Zero, One, Two, Three, Four, Six, Seven, Eight, Nine))
        assert(! a.isComplete)
        assert(a.isFinite)
        assert(a.isBounded)
        assert(a.hasLb)
        assert(a.hasUb)
        assertEq(a.maybeLb.get, Zero)
        assertEq(a.maybeUb.get, Nine)
        assertEq(a.lb, Zero)
        assertEq(a.ub, Nine)
        assertEq(a.hull.lb, Zero)
        assertEq(a.hull.ub, Nine)
        assertEq(a.hull.size, 10)
        assert(a.hasGaps)

        // [0, 9] \ {5, 7}
        assert(! b.isEmpty)
        assertEq(b.size, 8)
        assertEq(b.toString, "0..4 union {6} union 8..9")
        assert(! b.contains(Seven))
        assertEq(b.values.size, 8)
        assertEq(b.values.toList, List(Zero, One, Two, Three, Four, Six, Eight, Nine))
        assert(! b.isComplete)
        assert(b.isFinite)
        assert(b.isBounded)
        assert(b.hasLb)
        assert(b.hasUb)
        assertEq(b.maybeLb.get, Zero)
        assertEq(b.maybeUb.get, Nine)
        assertEq(b.lb, Zero)
        assertEq(b.ub, Nine)
        assertEq(b.hull.lb, Zero)
        assertEq(b.hull.ub, Nine)
        assertEq(b.hull.size, 10)
        assert(b.hasGaps)

        // [0, 9] \ {5, 6, 7}
        assert(! c.isEmpty)
        assertEq(c.size, 7)
        assertEq(c.toString, "0..4 union 8..9")
        assert(! c.contains(Six))
        assertEq(c.values.size, 7)
        assertEq(c.values.toList, List(Zero, One, Two, Three, Four, Eight, Nine))
        assert(! c.isComplete)
        assert(c.isFinite)
        assert(c.isBounded)
        assert(c.hasLb)
        assert(c.hasUb)
        assertEq(c.maybeLb.get, Zero)
        assertEq(c.maybeUb.get, Nine)
        assertEq(c.lb, Zero)
        assertEq(c.ub, Nine)
        assertEq(c.hull.lb, Zero)
        assertEq(c.hull.ub, Nine)
        assertEq(c.hull.size, 10)
        assert(c.hasGaps)

        // ]-inf, +inf[ \ ([0, 9] \ {5, 7})
        assertEq(d.toString, "-inf..-1 union {5} union {7} union 10..+inf")
        assert(! d.isEmpty)
        assertEx(d.size)
        assert(! d.isComplete)
        assert(! d.isFinite)
        assert(! d.isSingleton)
        assert(d.contains(Five))
        assertEx(d.singleValue)
        assertEx(d.values)
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
    def testEquality: Unit = {
        val sampleSize = 16
        val testData = helper.createTestData(baseRange, sampleSize).map(ensureRangeList)
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
    def testOrdering: Unit = {
        val sampleSize = 16
        val testData = helper.createTestData(baseRange, sampleSize).map(ensureRangeList)
        helper.testOrdering(testData)
    }

    @Test
    def testOperations: Unit = {
        val sampleSize = 8
        val testData = helper.createTestData(baseRange, sampleSize).map(ensureRangeList)
        val extendedBaseRange = IntegerRange(baseRange.lb - One, baseRange.ub + One)
        helper.testOperations(testData, extendedBaseRange.values.toSeq)
    }

    @Test
    def testRandomSubdomainCreation: Unit = {
        val testData = this.testData ++ IntegerDomainTestHelper.createEdgeCases(baseRange).map(ensureRangeList)
        helper.testRandomSubrangeCreation(testData)
        helper.testRandomSubdomainCreation(testData)
    }

}
