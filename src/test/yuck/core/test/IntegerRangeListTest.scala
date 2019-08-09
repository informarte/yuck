package yuck.core.test

import scala.collection._

import org.junit._

import yuck.core._
import yuck.util.testing.UnitTest

/**
 * @author Michael Marte
 *
 */
@Test
@FixMethodOrder(runners.MethodSorters.NAME_ASCENDING)
final class IntegerRangeListTest extends UnitTest {

    private val randomGenerator = new JavaRandomGenerator
    private val helper = new IntegerDomainTestHelper(randomGenerator, logger)
    private val baseRange = new IntegerRange(IntegerValue.get(-5), Five)
    private val testData = {
        val (a, _) = IntegerDomainPruner.ne(new IntegerRange(Zero, Nine), new IntegerRange(Five, Five))
        val (b, _) = IntegerDomainPruner.ne(a, new IntegerRange(Seven, Seven))
        val (c, _) = IntegerDomainPruner.ne(b, new IntegerRange(Six, Six))
        val d = new IntegerRangeList(CompleteIntegerRange).diff(b)
        List(a, b, c, d)
    }

    @Test
    def testRepresentation: Unit = {

        helper.testRepresentation((a, b) => new IntegerRangeList(a, b))
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
        val testData =
            (List(EmptyIntegerRange, new IntegerRange(Zero, Nine)) ++ helper.specialInfiniteRanges ++ this.testData)
                .map(IntegerDomain.ensureRangeList)
        helper.testEquality(testData)
        for (a <- testData) {
            val b = new IntegerRangeList(a.ranges)
            assertEq(a, b)
            assertEq(b, a)
            assertNe(a, False)
            assertNe(False, a)
        }
    }

    @Test
    def testOrdering: Unit = {
        val sampleSize = 16
        val testData = helper.createTestData(baseRange, sampleSize).map(IntegerDomain.ensureRangeList)
        helper.testOrdering(testData, IntegerRangeList.ordering)
    }

    @Test
    def testOperations: Unit = {
        val sampleSize = 8
        val testData = helper.createTestData(baseRange, sampleSize).map(IntegerDomain.ensureRangeList)
        val extendedBaseRange = new IntegerRange(baseRange.lb - One, baseRange.ub + One)
        helper.testOperations(testData, extendedBaseRange.values.toSeq)
    }

    @Test
    def testRandomSubrangeCreation: Unit = {
        val sampleSize = 1000
        val baseDomain = new IntegerRangeList(Vector(new IntegerRange(Zero, Four), new IntegerRange(Six, Nine)))
        val sample = new mutable.HashSet[IntegerRange]
        for (i <- 1 to sampleSize) {
            val e = baseDomain.randomSubrange(randomGenerator)
            assert(e.isSubsetOf(baseDomain))
            sample += e
        }
        assertGe(sample.size, baseDomain.size * (baseDomain.size + 1) / 4)
    }

}
