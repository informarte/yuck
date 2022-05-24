package yuck.core.test

import org.junit.*

import scala.annotation.tailrec

import yuck.core.*
import yuck.test.util.UnitTest

/**
 * @author Michael Marte
 *
 */
@FixMethodOrder(org.junit.runners.MethodSorters.NAME_ASCENDING)
class IntegerDomainPrunerTest extends UnitTest {

    @tailrec
    private def fixedPoint[State](f: State => State, u: State): State = {
        val v = f(u)
        if (u == v) u else fixedPoint(f, v)
    }

    import scala.language.implicitConversions
    implicit def toIntegerValue(i: Int): IntegerValue = IntegerValue(i)
    implicit def toIntegerRange(r: Range): IntegerRange = {
        require(r.step == 1)
        require(r.isInclusive)
        IntegerRange(r.start, r.end)
    }

    private val randomGenerator = new JavaRandomGenerator
    private val helper = new IntegerDomainTestHelper(randomGenerator, logger)
    private val testData = helper.createTestData(-5 to 5, 32)

    private def testEqRule(d: IntegerDomain, e: IntegerDomain): Unit = {
        assertEq(IntegerDomainPruner.eqRule(d, e), (d.intersect(e), d.intersect(e)))
    }

    @Test
    def testEqRule(): Unit = {
        for (d <- testData) {
            for (e <- testData) {
                testEqRule(d, e)
            }
        }
    }

    private def testNeRule(d: IntegerDomain, e: IntegerDomain): Unit = {
        val (f, g) = IntegerDomainPruner.neRule(d, e)
        assertEq(f, if (e.isSingleton) d.diff(e) else d)
        assertEq(g, if (d.isSingleton) e.diff(d) else e)
    }

    @Test
    def testNeRule(): Unit = {
        for (d <- testData) {
            for (e <- testData) {
                testNeRule(d, e)
            }
        }
    }

    private def testLeRule(d: IntegerDomain, e: IntegerDomain): Unit = {
        val (f, g) = IntegerDomainPruner.leRule(d, e)
        assert(f.isSubsetOf(d))
        assert(g.isSubsetOf(e))
        if (d.isEmpty || e.isEmpty || e.precedes(d)) {
            assert(f.isEmpty)
            assert(g.isEmpty)
        } else if (d.isFinite && e.isFinite) {
            d.values.foreach(a => assertEq(e.values.exists(b => a <= b), f.contains(a)))
            e.values.foreach(b => assertEq(d.values.exists(a => a <= b), g.contains(b)))
        } else {
            assert(! f.isEmpty)
            assert(! g.isEmpty)
            assertEq(f.lb, d.lb)
            assertEq(g.ub, e.ub)
            if (d.endsAfter(e)) {
                assert(! f.endsAfter(g))
                // Check that not too many values were pruned from d.
                // d.endsAfter(e) => e.ub is finite
                // e.ub is finite && g.ub == e.ub => g.ub is finite
                // ! f.endsAfter(g) && g.ub is finite => f.ub is finite
                assert(IntegerRange(f.ub + One, e.ub).intersect(d).isEmpty)
            } else {
                assertEq(f.ub, d.ub)
            }
            if (e.startsBefore(d)) {
                assert(! g.startsBefore(f))
                // Check that not too many values were pruned from e.
                // e.startsBefore(d) => d.lb.isFinite
                // d.lb is finite && f.lb == d.lb => f.lb is finite
                // ! g.startsBefore(f) && f.lb is finite => g.lb is finite
                assert(IntegerRange(d.lb, g.lb - One).intersect(e).isEmpty)
            } else {
                assertEq(g.lb, e.lb)
            }
        }
    }

    @Test
    def testLeRule(): Unit = {
        for (d <- testData) {
            for (e <- testData) {
                testLeRule(d, e)
            }
        }
    }

    private def testLtRule(d: IntegerDomain, e: IntegerDomain): Unit = {
        val (f, g) = IntegerDomainPruner.ltRule(d, e)
        assert(f.isSubsetOf(d))
        assert(g.isSubsetOf(e))
        if (d.isFinite && e.isFinite) {
            d.values.foreach(a => assertEq(e.values.exists(b => a < b), f.contains(a)))
            e.values.foreach(b => assertEq(d.values.exists(a => a < b), g.contains(b)))
        } else if (d.isEmpty || e.isEmpty || (d.hasLb && e.hasUb && e.ub <= d.lb)) {
            assert(f.isEmpty)
            assert(g.isEmpty)
        } else {
            assert(! f.isEmpty)
            assert(! g.isEmpty)
            assertEq(f.lb, d.lb)
            assertEq(g.ub, e.ub)
            if (d.endsBefore(e) || ! e.hasUb) {
                assertEq(f.ub, d.ub)
            } else {
                // e.ub is finite
                assert(f.endsBefore(e))
                // f.endsBefore(e) => f.ub is finite
                if (f.ub < e.ub - One) {
                    // Check that not too many values were pruned from d.
                    assert(IntegerRange(f.ub + One, e.ub - One).intersect(d).isEmpty)
                }
            }
            if (e.startsAfter(d) || ! d.hasLb) {
                assertEq(g.lb, e.lb)
            } else {
                // d.lb is finite
                assert(g.startsAfter(d))
                // g.startsAfter(d) => g.lb is finite
                if (g.lb > d.lb + One) {
                    // Check that not too many values were pruned from e.
                    assert(IntegerRange(d.lb + One, g.lb - One).intersect(e).isEmpty)
                }
            }
        }
    }

    @Test
    def testLtRule(): Unit = {
        for (d <- testData) {
            for (e <- testData) {
                testLtRule(d, e)
            }
        }
    }

    @Test
    def testMinRule(): Unit = {

        type State = (List[IntegerDomain], IntegerDomain)

        def minRule(u: State): State = {
            val (lhs0, rhs0) = u
            val (lhs1, rhs1) = IntegerDomainPruner.minRule(lhs0, rhs0)
            (lhs1.toList, rhs1)
        }

        def checkPruning(u: State, v: State): Unit = {
            assertEq(fixedPoint[State](minRule, u), v)
        }

        // ?- X in 1..3, Y in 2..5, Z #= min(X, Y).
        checkPruning((List(1 to 3, 2 to 5), CompleteIntegerRange), (List(1 to 3, 2 to 5), 1 to 3))

        // ?- X in 1..3, Y in 2..5, Z #> 2, Z #= min(X, Y).
        checkPruning((List(1 to 3, 2 to 5), IntegerRange(3, null)), (List(3 to 3, 3 to 5), 3 to 3))

        // empty domains
        checkPruning(
            (List(EmptyIntegerRange, 2 to 5), IntegerRange(3, null)),
            (List(EmptyIntegerRange, EmptyIntegerRange), EmptyIntegerRange))
        checkPruning(
            (List(1 to 3, EmptyIntegerRange), IntegerRange(3, null)),
            (List(EmptyIntegerRange, EmptyIntegerRange), EmptyIntegerRange))
        checkPruning(
            (List(1 to 3, 2 to 5), EmptyIntegerRange),
            (List(EmptyIntegerRange, EmptyIntegerRange), EmptyIntegerRange))

    }

    @Test
    def testMaxRule(): Unit = {

        type State = (List[IntegerDomain], IntegerDomain)

        def maxRule(u: State): State = {
            val (lhs0, rhs0) = u
            val (lhs1, rhs1) = IntegerDomainPruner.maxRule(lhs0, rhs0)
            (lhs1.toList, rhs1)
        }

        def checkPruning(u: State, v: State): Unit = {
            assertEq(fixedPoint[State](maxRule, u), v)
        }

        // ?- X in 1..3, Y in 2..5, Z #= max(X, Y).
        checkPruning((List(1 to 3, 2 to 5), CompleteIntegerRange), (List(1 to 3, 2 to 5), 2 to 5))

        // ?- X in 1..3, Y in 2..5, Z #< 3, Z #= max(X, Y).
        checkPruning((List(1 to 3, 2 to 5), IntegerRange(null, 2)), (List(1 to 2, 2 to 2), 2 to 2))

        // empty domains
        checkPruning(
            (List(EmptyIntegerRange, 2 to 5), IntegerRange(3, null)),
            (List(EmptyIntegerRange, EmptyIntegerRange), EmptyIntegerRange))
        checkPruning(
            (List(1 to 3, EmptyIntegerRange), IntegerRange(3, null)),
            (List(EmptyIntegerRange, EmptyIntegerRange), EmptyIntegerRange))
        checkPruning(
            (List(1 to 3, 2 to 5), EmptyIntegerRange),
            (List(EmptyIntegerRange, EmptyIntegerRange), EmptyIntegerRange))

    }

    private def testAbsRule(d: IntegerDomain, e0: IntegerDomain): Unit = {
        val e = e0.intersect(NonNegativeIntegerRange)
        val (f, g) = IntegerDomainPruner.absRule(d, e0)
        assert(f.isSubsetOf(d))
        assert(g.isSubsetOf(e))
        assert(! (d.isEmpty || e.isEmpty) || (f.isEmpty && g.isEmpty))
        assert(! (d.isFinite || e.isFinite) || (f.isFinite && g.isFinite))
        assert(g.intersect(NegativeIntegerRange).isEmpty)
        assertEq(g, f.intersect(NegativeIntegerRange).mirrored.union(f.intersect(NonNegativeIntegerRange)))
        if (d.isFinite) {
            assert(d.valuesIterator.forall(a => f.contains(a) == e.contains(a.abs)))
        } else {
            assertEq(f, d.intersect(e.union(e.mirrored)))
        }
        if (e.isFinite) {
            assert(e.valuesIterator.forall(a => g.contains(a) == (d.contains(a) || d.contains(a.negated))))
        } else {
            assertEq(g, e.intersect(d.union(d.mirrored)))
        }
    }

    @Test
    def testAbsRule(): Unit = {
        for (d <- testData) {
            for (e <- testData) {
                testAbsRule(d, e)
            }
        }
    }

    @Test
    def testLinEqRule(): Unit = {

        type LinearCombination = List[(IntegerValue, IntegerDomain)]
        type State = (LinearCombination, IntegerDomain)

        def linEqRule(u: State): State = {
            val (lhs0, rhs0) = u
            val (lhs1, rhs1) = IntegerDomainPruner.linEqRule(lhs0, rhs0)
            (lhs0.iterator.map(_._1).zip(lhs1.iterator).toList, rhs1)
        }

        def checkPruning(u: State, v: State): Unit = {
            assertEq(fixedPoint[State](linEqRule, u), v)
        }

        val lhs1: LinearCombination = List((3, 1 to 10))
        val lhs2: LinearCombination = List((2, 1 to 10), (-3, -10 to -1))

        // ?- X in 1..10, 3 * X #= Y.
        checkPruning((lhs1, CompleteIntegerRange), (lhs1, 3 to 30))

        // ?- X in 1..10, 3 * X #= Y, Y #>= 4.
        checkPruning((lhs1, IntegerRange(4, null)), (List((3, 2 to 10)), 6 to 30))

        // ?- X in 1..10, 3 * X #= Y, Y #=< 10.
        checkPruning((lhs1, IntegerRange(null, 10)), (List((3, 1 to 3)), 3 to 9))

        // ?- X in 1..10, 3 * X #= Y, Y #>= 4, Y #=< 10.
        checkPruning((lhs1, 4 to 10), (List((3, 2 to 3)), 6 to 9))

        // ?- X in 1..10, Y in (-10)..(-1), 2 * X - 3 * Y #= Z.
        checkPruning((lhs2, CompleteIntegerRange), (lhs2, 5 to 50))

        // ?- X in 1..10, Y in (-10)..(-1), 2 * X - 3 * Y #= Z, Z #>= 50.
        checkPruning((lhs2, IntegerRange(50, null)), (List((2, 10 to 10), (-3, -10 to -10)), 50 to 50))

        // ?- X in 1..10, Y in (-10)..(-1), 2 * X - 3 * Y #= Z, Z #=< 5.
        checkPruning((lhs2, IntegerRange(null, 5)), (List((2, 1 to 1), (-3, -1 to -1)), 5 to 5))

        // ?- X in 1..10, Y in (-10)..(-1), 2 * X - 3 * Y #= Z, Z #= 6.
        checkPruning((lhs2, 6 to 6), (List((2, EmptyIntegerRange), (-3, EmptyIntegerRange)), EmptyIntegerRange))

        // ?- X in 1..10, Y in (-10)..(-1), 2 * X - 3 * Y #= Z, Z #= 12.
        checkPruning((lhs2, 12 to 12), (List((2, 3 to 3), (-3, -2 to -2)), 12 to 12))

        // empty lhs
        checkPruning((Nil, CompleteIntegerRange), (Nil, 0 to 0))

        // scalar 0
        checkPruning(
             (List((Zero, OneToOneIntegerRange)), ZeroToZeroIntegerRange),
             (List((Zero, OneToOneIntegerRange)), ZeroToZeroIntegerRange))

        // empty domains
        checkPruning((lhs2, EmptyIntegerRange), (lhs2.map{case (a, _) => (a, EmptyIntegerRange)}, EmptyIntegerRange))
        checkPruning(
            (List((2, EmptyIntegerRange), (-3, -10 to -1)), CompleteIntegerRange),
            ((List((2, EmptyIntegerRange), (-3, EmptyIntegerRange)), EmptyIntegerRange)))

        // integer overflow handling
        checkPruning(
            (List((2, IntegerRange(Long.MaxValue, Long.MaxValue))), ZeroToOneIntegerRange),
            (List((2, IntegerRange(Long.MaxValue, Long.MaxValue))), ZeroToOneIntegerRange))

    }

    @Test
    def testTimesRule(): Unit = {

        type State = (IntegerDomain, IntegerDomain, IntegerDomain)

        def timesRule(u: State): State = {
            val (dx0, dy0, dz0) = u
            IntegerDomainPruner.timesRule(dx0, dy0, dz0)
        }

        def checkPruning(u: State, v: State): Unit = {
            assertEq(fixedPoint[State](timesRule, u), v)
        }

        // ?- X in 1..20, Y in 9..11, Z in 155..161, X * Y #= Z.
        checkPruning((1 to 20, 9 to 11, 155 to 161), (16 to 16, 10 to 10, 160 to 160))

        // ?- X in 3..10, Y in -2..2, Z in -155..161, X * Y #= Z.
        checkPruning((3 to 10, -2 to 2, -155 to 161), (3 to 10, -2 to 2, -20 to 20))

        // ?- X in 2..3, Y in 2..3, Z in 5..6, X * Y #= Z.
        checkPruning((2 to 3, 2 to 3, 5 to 6), (2 to 3, 2 to 3, 5 to 6))

        // ?- X in 0..2, Y in -1..2, Z in 3..4, X * Y #= Z.
        checkPruning((0 to 2, -1 to 2, 3 to 4), (2 to 2, 2 to 2, 4 to 4))

        // empty domains
        checkPruning((EmptyIntegerRange, -1 to 2, 3 to 4), (EmptyIntegerRange, EmptyIntegerRange, EmptyIntegerRange))
        checkPruning((0 to 2, EmptyIntegerRange, 3 to 4), (EmptyIntegerRange, EmptyIntegerRange, EmptyIntegerRange))
        checkPruning((0 to 2, -1 to 2, EmptyIntegerRange), (EmptyIntegerRange, EmptyIntegerRange, EmptyIntegerRange))

    }

}
