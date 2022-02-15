package yuck.core.test

import org.junit.*

import yuck.core.*
import yuck.test.util.UnitTest

/**
 * @author Michael Marte
 *
 */
@FixMethodOrder(runners.MethodSorters.NAME_ASCENDING)
final class IntegerSetValueTest extends UnitTest with IntegerSetValueTestData {

    override protected val randomGenerator = new JavaRandomGenerator
    private val helper = new OrderedValueTestHelper[IntegerSetValue](randomGenerator)

    @Test
    def testConstruction(): Unit = {
        for (a <- baseData) {
            assertEq(new IntegerSetValue(a).set, a)
        }
    }

    @Test
    def testSpecialValues(): Unit = {
        assert(EmptyIntegerSetValue.set.isEmpty)
        assert(CompleteIntegerSetValue.set.isComplete)
    }

    @Test
    def testEquality(): Unit = {
        helper.testEquality(testData)
        for (a <- testData) {
            val b = new IntegerSetValue(a.set)
            assertEq(a, b)
            assertEq(b, a)
            assertNe(a, Zero)
            assertNe(Zero, a)
            for (b <- testData) {
                assert(if (a.eq(b)) a == b else a != b)
            }
        }
    }

    private def valuesIterator(range: IntegerRange): Iterator[IntegerValue] =
        if (range.isFinite) range.valuesIterator
        else Iterator.from(range.lb.value).map(IntegerValue.apply)

    private def valuesIterator(ranges: Iterable[IntegerRange]): Iterator[IntegerValue] =
        ranges.iterator.flatMap(valuesIterator)

    private def valuesIterator(a: IntegerSetValue): Iterator[IntegerValue] =
        valuesIterator(IntegerDomain.ensureRangeList(a.set).ranges)

    private def compareDomainsWithoutLowerBound(a: IntegerDomain, b: IntegerDomain): Int = {
        require(! a.hasLb)
        require(! b.hasLb)
        val c = IntegerDomain.ensureRangeList(a).ranges
        val d = IntegerDomain.ensureRangeList(b).ranges
        val lb = IntegerValue.min(c.head.ub, d.head.ub)
        val e = c.tail.prepended(IntegerRange(lb, c.head.ub))
        val f = d.tail.prepended(IntegerRange(lb, d.head.ub))
        valuesIterator(e).compare(valuesIterator(f))
    }

    @Test
    def testOrdering(): Unit = {
        helper.testOrdering(testData)
        // We test by comparing iterators over values.
        for (a <- testData) {
            for (b <- testData) {
                val result = a.compare(b).sign
                if (a.set == b.set) {
                    // 1..2
                    // 1..2
                    assertEq(result, 0)
                } else if (a.set.isEmpty) {
                    // {}
                    // 1..2
                    assertEq(result, -1)
                } else if (b.set.isEmpty) {
                    // 1..2
                    // {}
                    assertEq(result, 1)
                } else if (a.set.isFinite && b.set.isFinite) {
                    // 1..2
                    // -4..2
                    assertEq(result, valuesIterator(a).compare(valuesIterator(b)).sign)
                } else if (a.set.hasLb && b.set.hasLb) {
                    // 1..2
                    // {0} union 3..+inf
                    assertEq(result, valuesIterator(a).compare(valuesIterator(b)).sign)
                } else if (a.set.hasLb) {
                    // 1..2
                    // -inf..0 union 3..+inf
                    assertEq(result, 1)
                } else if (b.set.hasLb) {
                    // -inf..0 union 3..+inf
                    // 1..2
                    assertEq(result, -1)
                } else if (a.set.hasUb && b.set.hasUb) {
                    // -inf..-1
                    // -inf..-5
                    assertEq(result, compareDomainsWithoutLowerBound(a.set, b.set).sign)
                } else if (a.set.hasUb) {
                    if (b.set.isComplete) {
                        if (a.set.hasGaps) {
                            // -inf..-5 union -2..-1
                            // -inf..+inf
                            assertEq(result, 1)
                        } else {
                            // -inf..-1
                            // -inf..+inf
                            assertEq(result, -1)
                        }
                    } else {
                        // -inf..-1
                        // -inf..0 union 3..+inf
                        assertEq(result, compareDomainsWithoutLowerBound(a.set, b.set).sign)
                    }
                } else if (b.set.hasUb) {
                    if (a.set.isComplete) {
                        if (b.set.hasGaps) {
                            // -inf..+inf
                            // -inf..-5 union -2..-1
                            assertEq(result, -1)
                        } else {
                            // -inf..+inf
                            // -inf..-1
                            assertEq(result, 1)
                        }
                    } else {
                        // -inf..0 union 3..+inf
                        // -inf..-1
                        assertEq(result, compareDomainsWithoutLowerBound(a.set, b.set).sign)
                    }
                } else if (a.set.isComplete) {
                    // -inf..+inf
                    // -inf..0 union 3..+inf
                    assertEq(result, -1)
                } else if (b.set.isComplete) {
                    // -inf..0 union 3..+inf
                    // -inf..+inf
                    assertEq(result, 1)
                } else {
                    // -inf..0 union 3..+inf
                    // -inf..-5 union 3..+inf
                    assertEq(result, compareDomainsWithoutLowerBound(a.set, b.set).sign)
                }
            }
        }
    }

    @Test
    def testConfiguration(): Unit = {
        import IntegerSetValue.*
        assertEq(valueTraits, IntegerSetValueTraits)
        assertEq(valueOrdering, IntegerSetValueOrdering)
        assertEq(domainOrdering, IntegerSetDomainOrdering)
    }

}
