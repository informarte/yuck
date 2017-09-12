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

    private val helper = new IntegerDomainTestHelper(logger)

    @Test
    def testBasics {
        val randomGenerator = new JavaRandomGenerator

        helper.testRangeBasics(randomGenerator, (a, b) => new IntegerRangeList(a, b))

        // [0, 9] \ {5}
        val a = IntegerDomainPruner.ne(new IntegerRange(Zero, Nine), Five)
        assertNe(a, "")
        assertEq(a, a)
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
        val b = IntegerDomainPruner.ne(a, Seven)
        assertNe(b, "")
        assertEq(b, b)
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
        val c = IntegerDomainPruner.ne(b, Six)
        assertNe(c, "")
        assertEq(c, c)
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
        val d = new IntegerRangeList(CompleteIntegerRange).diff(b)
        assertNe(d, "")
        assertEq(d, d)
        assertEq(d, new IntegerRangeList(Vector(new IntegerRange(null, MinusOne), new IntegerRange(Five, Five), new IntegerRange(Seven, Seven), new IntegerRange(Ten, null))))
        assertEq(d.toString, "-inf..-1 union {5} union {7} union 10..+inf")
        assert(! d.isEmpty)
        assertEx(d.size)
        assert(! d.isComplete)
        assert(! d.isFinite)
        assert(! d.isSingleton)
        assert(d.contains(Five))
        assertEx(d.singleValue)
        assertEx(d.randomValue(randomGenerator))
        assertEx(d.nextRandomValue(randomGenerator, Five))
        assertEx(d.values)
        assert(! d.isBounded)
        assert(! d.hasLb)
        assert(! d.hasUb)
        assert(d.maybeLb.isEmpty)
        assert(d.maybeUb.isEmpty)
        assertEq(d.lb, null)
        assertEq(d.ub, null)
        assert(d.hasGaps)

        // equality
        val testData = (List(EmptyIntegerRange, new IntegerRange(Zero, Nine)) ++ helper.specialInfiniteRanges ++ List(a, b, c, d)).map(IntegerDomain.ensureRangeList)
        for (d <- testData) {
            assertEq(d, d)
        }
        for (List(d, e) <- testData.combinations(2)) {
            assertNe(d, e)
        }

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
        val testData = (randomFiniteRangeLists ++ randomInfiniteRangeLists ++ edgeCases ++ edgeCases).map(IntegerDomain.ensureRangeList)
        helper.testOrdering(testData, IntegerRangeList.ordering)
    }

    @Test
    def testRandomSubrangeCreation {
        val SAMPLE_SIZE = 1000
        val randomGenerator = new JavaRandomGenerator
        val baseDomain = new IntegerRangeList(Vector(new IntegerRange(Zero, Four), new IntegerRange(Six, Nine)))
        val sample = new mutable.HashSet[IntegerRange]
        for (i <- 1 to SAMPLE_SIZE) {
            val e = baseDomain.randomSubrange(randomGenerator)
            assert(e.isSubsetOf(baseDomain))
            sample += e
        }
        assertGe(sample.size, baseDomain.size * (baseDomain.size + 1) / 4)
    }

}
