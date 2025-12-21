package yuck.core.test

import org.junit.jupiter.api.Test

import yuck.core.test.util.OrderingTestTooling
import yuck.core.{*, given}
import yuck.test.util.UnitTest

final class IntegerSetValueTest
    extends UnitTest
       with OrderingTestTooling[IntegerSetValue]
       with IntegerSetValueTestData
{

    override protected val randomGenerator = new JavaRandomGenerator

    @Test
    def testConstruction(): Unit = {
        for a <- baseData do {
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
        val testData = this.testData.distinct
        testEquality(testData)
        for a <- testData do {
            val b = new IntegerSetValue(a.set)
            assertEq(a, b)
            assertEq(b, a)
            assertNe(a, Zero)
            assertNe(Zero, a)
            for b <- testData do {
                assert(if a.eq(b) then a == b else a != b)
            }
        }
    }

    private def valuesIterator(range: IntegerRange): Iterator[IntegerValue] =
        if range.isFinite
        then range.valuesIterator
        else Iterator.from(range.lb.toInt).map(IntegerValue.apply)

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
        testOrdering(testData)
        // We test by comparing iterators over values.
        for a <- testData do {
            for b <- testData do {
                val result = a.compare(b).sign
                if a.set == b.set then {
                    // 1..2
                    // 1..2
                    assertEq(result, 0)
                } else if a.set.isEmpty then {
                    // {}
                    // 1..2
                    assertEq(result, -1)
                } else if b.set.isEmpty then {
                    // 1..2
                    // {}
                    assertEq(result, 1)
                } else if a.set.isFinite && b.set.isFinite then {
                    // 1..2
                    // -4..2
                    assertEq(result, valuesIterator(a).compare(valuesIterator(b)).sign)
                } else if a.set.hasLb && b.set.hasLb then {
                    // 1..2
                    // {0} union 3..+inf
                    assertEq(result, valuesIterator(a).compare(valuesIterator(b)).sign)
                } else if a.set.hasLb then {
                    // 1..2
                    // -inf..0 union 3..+inf
                    assertEq(result, 1)
                } else if b.set.hasLb then {
                    // -inf..0 union 3..+inf
                    // 1..2
                    assertEq(result, -1)
                } else if a.set.hasUb && b.set.hasUb then {
                    // -inf..-1
                    // -inf..-5
                    assertEq(result, compareDomainsWithoutLowerBound(a.set, b.set).sign)
                } else if a.set.hasUb then {
                    if b.set.isComplete then {
                        if a.set.hasGaps then {
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
                } else if b.set.hasUb then {
                    if a.set.isComplete then {
                        if b.set.hasGaps then {
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
                } else if a.set.isComplete then {
                    // -inf..+inf
                    // -inf..0 union 3..+inf
                    assertEq(result, -1)
                } else if b.set.isComplete then {
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
        import IntegerSetValue.given
        def testOrdering()(using ordering: Ordering[IntegerSetValue]) = {
            assertEq(ordering, IntegerSetValueOrdering)
        }
        testOrdering()
        def testTraits()(using traits: OrderedValueTraits[IntegerSetValue]) = {
            assertEq(traits, IntegerSetValueTraits)
        }
        testTraits()
    }

}
