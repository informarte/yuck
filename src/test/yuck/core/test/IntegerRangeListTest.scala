package yuck.core.test

import org.junit.jupiter.api.Test
import org.junit.jupiter.api.parallel.{Execution, ExecutionMode}

import yuck.core.*
import yuck.core.IntegerDomain.ensureRangeList
import yuck.test.*
import yuck.test.util.UnitTest

@Execution(ExecutionMode.CONCURRENT)
final class IntegerRangeListTest extends UnitTest with IntegerDomainTestTooling {

    private val baseRange = IntegerRange(-5, 5)

    override protected val randomGenerator = new JavaRandomGenerator

    @Test
    def testRepresentation(): Unit = {

        testRangeRepresentation((a, b) => IntegerRangeList(a, b))
        testFiniteRepresentationWithGaps(values => ensureRangeList(IntegerDomain(values)))

        // ]-inf, +inf[ \ ([0, 9] \ {5, 7})
        val d = IntegerRangeList(Vector(IntegerRange(null, MinusOne), IntegerRange(5, 5), IntegerRange(7, 7), IntegerRange(Ten, null)))
        assertEq(d.toString, "-inf..-1 ∪ {5} ∪ {7} ∪ 10..+inf")
        assert(! d.isEmpty)
        assertThrows(d.size)
        assert(! d.isComplete)
        assert(! d.isFinite)
        assert(! d.isSingleton)
        assert(d.contains(Five))
        assertThrows(d.singleValue)
        assertThrows(d.values)
        assertThrows(d.valuesIterator)
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
        val testData = createRangeLists(baseRange, 32)
        testEquality(testData)
        for d <- testData do {
            val e = IntegerRangeList(d.ranges)
            assertEq(d, e)
            assertEq(e, d)
            assertNe(d, False)
            assertNe(False, d)
            for e <- testData do {
                assert(if d.eq(e) then d == e else d != e)
            }
        }
    }

    @Test
    def testOrdering(): Unit = {
        val testData = createRangeLists(baseRange, 32)
        testOrdering(testData)
    }

    @Test
    def testOperations(): Unit = {
        val testDomains = createRangeLists(baseRange, 16)
        val testValues = IntegerRange(baseRange.lb - One, baseRange.ub + One).values.toSeq
        testUnaryOperations(testDomains, testValues)
        testBinaryOperations(testDomains)
    }

    @Test
    def testRandomSubdomainCreation(): Unit = {
        val testData = createRangeLists(baseRange, 16)
        testRandomSubrangeCreation(testData)
        testRandomSubdomainCreation(testData)
    }

}
