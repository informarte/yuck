package yuck.core.test

import org.junit._

import yuck.core._
import yuck.util.testing.UnitTest

/**
 * @author Michael Marte
 *
 */
@FixMethodOrder(org.junit.runners.MethodSorters.NAME_ASCENDING)
class IntegerDomainPrunerTest extends UnitTest {

    private def fixedPoint[State](f: State => State, u: State): State = {
        val v = f(u)
        if (u == v) u else fixedPoint(f, v)
    }

    import scala.language.implicitConversions
    import IntegerDomain.createRange
    implicit def toIntegerValue(i: Int): IntegerValue = IntegerValue.get(i)
    implicit def toIntegerRange(r: Range): IntegerRange = {
        require(r.step == 1)
        require(r.isInclusive)
        createRange(r.start, r.end)
    }

    private val randomGenerator = new JavaRandomGenerator
    private val helper = new IntegerDomainTestHelper(randomGenerator, logger)
    private val testData = helper.createTestData(baseRange = -5 to 5, sampleSize = 32)

    private def testEqPruning(d: IntegerDomain, e: IntegerDomain) {
        assertEq(IntegerDomainPruner.eq(d, e), (d.intersect(e), d.intersect(e)))
    }

    @Test
    def testEqPruning {
        for (d <- testData) {
            for (e <- testData) {
                testEqPruning(d, e)
            }
        }
    }

    private def testNePruning(d: IntegerDomain, e: IntegerDomain) {
        val (f, g) = IntegerDomainPruner.ne(d, e)
        assertEq(f, if (e.isSingleton) d.diff(e) else d)
        assertEq(g, if (d.isSingleton) e.diff(d) else e)
    }

    @Test
    def testNePruning {
        for (d <- testData) {
            for (e <- testData) {
                testNePruning(d, e)
            }

        }
    }

    private def testLePruning(d: IntegerDomain, e: IntegerDomain) {
        val (f, g) = IntegerDomainPruner.le(d, e)
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
                assert(new IntegerRange(f.ub + One, e.ub).intersect(d).isEmpty)
            } else {
                assertEq(f.ub, d.ub)
            }
            if (e.startsBefore(d)) {
                assert(! g.startsBefore(f))
                // Check that not too many values were pruned from e.
                // e.startsBefore(d) => d.lb.isFinite
                // d.lb is finite && f.lb == d.lb => f.lb is finite
                // ! g.startsBefore(f) && f.lb is finite => g.lb is finite
                assert(new IntegerRange(d.lb, g.lb - One).intersect(e).isEmpty)
            } else {
                assertEq(g.lb, e.lb)
            }
        }
    }

    @Test
    def testLePruning {
        for (d <- testData) {
            for (e <- testData) {
                testLePruning(d, e)
            }
        }
    }

    private def testLtPruning(d: IntegerDomain, e: IntegerDomain) {
        val (f, g) = IntegerDomainPruner.lt(d, e)
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
                    assert(new IntegerRange(f.ub + One, e.ub - One).intersect(d).isEmpty)
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
                    assert(new IntegerRange(d.lb + One, g.lb - One).intersect(e).isEmpty)
                }
            }
        }
    }

    @Test
    def testLtPruning {
        for (d <- testData) {
            for (e <- testData) {
                testLtPruning(d, e)
            }
        }
    }

    @Test
    def testMinPruning {

        type State = (List[IntegerDomain], IntegerDomain)

        def min(u: State): State = {
            val (lhs0, rhs0) = u
            val (lhs1, rhs1) = IntegerDomainPruner.min(lhs0, rhs0)
            (lhs1.toList, rhs1)
        }

        def checkPruning(u: State, v: State) {
            assertEq(fixedPoint[State](min, u), v)
        }

        // ?- X in 1..3, Y in 2..5, Z #= min(X, Y).
        checkPruning((List(1 to 3, 2 to 5), CompleteIntegerRange), (List(1 to 3, 2 to 5), 1 to 3))

        // ?- X in 1..3, Y in 2..5, Z #> 2, Z #= min(X, Y).
        checkPruning((List(1 to 3, 2 to 5), createRange(3, null)), (List(3 to 3, 3 to 5), 3 to 3))

        // empty domains
        checkPruning(
            (List(EmptyIntegerRange, 2 to 5), createRange(3, null)),
            (List(EmptyIntegerRange, EmptyIntegerRange), EmptyIntegerRange))
        checkPruning(
            (List(1 to 3, EmptyIntegerRange), createRange(3, null)),
            (List(EmptyIntegerRange, EmptyIntegerRange), EmptyIntegerRange))
        checkPruning(
            (List(1 to 3, 2 to 5), EmptyIntegerRange),
            (List(EmptyIntegerRange, EmptyIntegerRange), EmptyIntegerRange))

    }

    @Test
    def testMaxPruning {

        type State = (List[IntegerDomain], IntegerDomain)

        def max(u: State): State = {
            val (lhs0, rhs0) = u
            val (lhs1, rhs1) = IntegerDomainPruner.max(lhs0, rhs0)
            (lhs1.toList, rhs1)
        }

        def checkPruning(u: State, v: State) {
            assertEq(fixedPoint[State](max, u), v)
        }

        // ?- X in 1..3, Y in 2..5, Z #= max(X, Y).
        checkPruning((List(1 to 3, 2 to 5), CompleteIntegerRange), (List(1 to 3, 2 to 5), 2 to 5))

        // ?- X in 1..3, Y in 2..5, Z #< 3, Z #= max(X, Y).
        checkPruning((List(1 to 3, 2 to 5), createRange(null, 2)), (List(1 to 2, 2 to 2), 2 to 2))

        // empty domains
        checkPruning(
            (List(EmptyIntegerRange, 2 to 5), createRange(3, null)),
            (List(EmptyIntegerRange, EmptyIntegerRange), EmptyIntegerRange))
        checkPruning(
            (List(1 to 3, EmptyIntegerRange), createRange(3, null)),
            (List(EmptyIntegerRange, EmptyIntegerRange), EmptyIntegerRange))
        checkPruning(
            (List(1 to 3, 2 to 5), EmptyIntegerRange),
            (List(EmptyIntegerRange, EmptyIntegerRange), EmptyIntegerRange))

    }

    @Test
    def testLinEqPruning {

        type LinearCombination = List[(IntegerValue, IntegerDomain)]
        type State = (LinearCombination, IntegerDomain)

        def linEq(u: State): State = {
            val (lhs0, rhs0) = u
            val (lhs1, rhs1) = IntegerDomainPruner.linEq(lhs0, rhs0)
            (lhs0.toIterator.map(_._1).zip(lhs1.toIterator).toList, rhs1)
        }

        def checkPruning(u: State, v: State) {
            assertEq(fixedPoint[State](linEq, u), v)
        }

        val lhs1: LinearCombination = List((3, 1 to 10))
        val lhs2: LinearCombination = List((2, 1 to 10), (-3, -10 to -1))

        // ?- X in 1..10, 3 * X #= Y.
        checkPruning((lhs1, CompleteIntegerRange), (lhs1, 3 to 30))

        // ?- X in 1..10, 3 * X #= Y, Y #>= 4.
        checkPruning((lhs1, createRange(4, null)), (List((3, 2 to 10)), 6 to 30))

        // ?- X in 1..10, 3 * X #= Y, Y #=< 10.
        checkPruning((lhs1, createRange(null, 10)), (List((3, 1 to 3)), 3 to 9))

        // ?- X in 1..10, 3 * X #= Y, Y #>= 4, Y #=< 10.
        checkPruning((lhs1, 4 to 10), (List((3, 2 to 3)), 6 to 9))

        // ?- X in 1..10, Y in (-10)..(-1), 2 * X - 3 * Y #= Z.
        checkPruning((lhs2, CompleteIntegerRange), (lhs2, 5 to 50))

        // ?- X in 1..10, Y in (-10)..(-1), 2 * X - 3 * Y #= Z, Z #>= 50.
        checkPruning((lhs2, createRange(50, null)), (List((2, 10 to 10), (-3, -10 to -10)), 50 to 50))

        // ?- X in 1..10, Y in (-10)..(-1), 2 * X - 3 * Y #= Z, Z #=< 5.
        checkPruning((lhs2, createRange(null, 5)), (List((2, 1 to 1), (-3, -1 to -1)), 5 to 5))

        // ?- X in 1..10, Y in (-10)..(-1), 2 * X - 3 * Y #= Z, Z #= 6.
        checkPruning((lhs2, 6 to 6), (List((2, EmptyIntegerRange), (-3, EmptyIntegerRange)), EmptyIntegerRange))

        // ?- X in 1..10, Y in (-10)..(-1), 2 * X - 3 * Y #= Z, Z #= 12.
        checkPruning((lhs2, 12 to 12), (List((2, 3 to 3), (-3, -2 to -2)), 12 to 12))

        // empty lhs
        checkPruning((Nil, CompleteIntegerRange), (Nil, 0 to 0))

        // empty domains
        checkPruning((lhs2, EmptyIntegerRange), (lhs2.map{case (a, _) => (a, EmptyIntegerRange)}, EmptyIntegerRange))
        checkPruning(
            (List((2, EmptyIntegerRange), (-3, -10 to -1)), CompleteIntegerRange),
            ((List((2, EmptyIntegerRange), (-3, EmptyIntegerRange)), EmptyIntegerRange)))

    }

    @Test
    def testTimesPruning {

        type State = (IntegerDomain, IntegerDomain, IntegerDomain)

        def times(u: State): State = {
            val (dx0, dy0, dz0) = u
            IntegerDomainPruner.times(dx0, dy0, dz0)
        }

        def checkPruning(u: State, v: State) {
            assertEq(fixedPoint[State](times, u), v)
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
