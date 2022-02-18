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
class BooleanDomainPrunerTest extends UnitTest {

    @tailrec
    private def fixedPoint[State](f: State => State, u: State): State = {
        val v = f(u)
        if (u == v) u else fixedPoint(f, v)
    }

    private def testPruning
        [InputDomain >: BooleanDomain <: Domain[BooleanValue]]
        (prune: (InputDomain, InputDomain) => (BooleanDomain, BooleanDomain),
         predicate: (BooleanValue, BooleanValue) => Boolean): Unit =
    {
        val testData = List(EmptyBooleanDomain, FalseDomain, TrueDomain, CompleteBooleanDomain)
        for (d <- testData) {
            for (e <- testData) {
                val feasible = d.values.exists(a => e.values.exists(b => predicate(a, b)))
                val (f, g) = prune(d, e)
                assert(f.isSubsetOf(d))
                assert(g.isSubsetOf(e))
                assertEq(f.isEmpty || g.isEmpty, ! feasible)
                if (feasible) {
                    for (a <- d.values) {
                        assertEq(e.values.exists(b => predicate(a, b)), f.contains(a))
                    }
                    for (b <- e.values) {
                        assertEq(d.values.exists(a => predicate(a, b)), g.contains(b))
                    }
                }
            }
        }
    }

    @Test
    def testEqRule(): Unit = {
        testPruning(BooleanDomainPruner.eqRule, (a, b) => a.truthValue == b.truthValue)
    }

    @Test
    def testNeRule(): Unit = {
        testPruning(BooleanDomainPruner.neRule, (a, b) => a.truthValue != b.truthValue)
    }

    @Test
    def testLeRule(): Unit = {
        testPruning(BooleanDomainPruner.leRule, (a, b) => ! a.truthValue || b.truthValue)
    }

    @Test
    def testLtRule(): Unit = {
        testPruning(BooleanDomainPruner.ltRule, (a, b) => ! a.truthValue && b.truthValue)
    }

    @Test
    def testConjunctionRule(): Unit = {

        type State = (Iterable[BooleanDomain], BooleanDomain)

        def linEqRule(u: State): State = {
            val (lhs0, rhs0) = u
            val (lhs1, rhs1) = BooleanDomainPruner.conjunctionRule(lhs0, rhs0)
            (lhs1.toList, rhs1)
        }

        def checkPruning(u: State, v: State): Unit = {
            val (lhs0, rhs0) = fixedPoint[State](linEqRule, u)
            val (lhs1, rhs1) = v
            for ((d0, d1) <- lhs0.view.zip(lhs1)) {
                assertEq(d0, d1)
            }
            assertEq(rhs0, rhs1)
        }

        // propagate from rhs to lhs: enforce conjunction
        checkPruning(
            (List(CompleteBooleanDomain), TrueDomain),
            (List(TrueDomain), TrueDomain))
        checkPruning(
            (List(FalseDomain, CompleteBooleanDomain), TrueDomain),
            (List(EmptyBooleanDomain, EmptyBooleanDomain), EmptyBooleanDomain))

        // propagate from rhs to lhs: enforce negation of conjunction
        checkPruning(
            (List(CompleteBooleanDomain), FalseDomain),
            (List(FalseDomain), FalseDomain))
        checkPruning(
            (List(TrueDomain), FalseDomain),
            (List(EmptyBooleanDomain), EmptyBooleanDomain))
        checkPruning(
            (List(FalseDomain, TrueDomain), FalseDomain),
            (List(FalseDomain, TrueDomain), FalseDomain))
        checkPruning(
            (List(FalseDomain, CompleteBooleanDomain), FalseDomain),
            (List(FalseDomain, CompleteBooleanDomain), FalseDomain))
        checkPruning(
            (List(TrueDomain, CompleteBooleanDomain), FalseDomain),
            (List(TrueDomain, FalseDomain), FalseDomain))

        // propagate from lhs to rhs
        checkPruning(
            (List(TrueDomain), CompleteBooleanDomain),
            (List(TrueDomain), TrueDomain))
        checkPruning(
            (List(FalseDomain, TrueDomain), CompleteBooleanDomain),
            (List(FalseDomain, TrueDomain), FalseDomain))

        // empty domains
        checkPruning(
            (List(FalseDomain, TrueDomain, CompleteBooleanDomain), EmptyBooleanDomain),
            (List(EmptyBooleanDomain, EmptyBooleanDomain, EmptyBooleanDomain), EmptyBooleanDomain))
        checkPruning(
            (List(FalseDomain, TrueDomain, EmptyBooleanDomain), FalseDomain),
            (List(EmptyBooleanDomain, EmptyBooleanDomain, EmptyBooleanDomain), EmptyBooleanDomain))

    }

    @Test
    def testDisjunctionRule(): Unit = {

        type State = (Iterable[BooleanDomain], BooleanDomain)

        def linEqRule(u: State): State = {
            val (lhs0, rhs0) = u
            val (lhs1, rhs1) = BooleanDomainPruner.disjunctionRule(lhs0, rhs0)
            (lhs1.toList, rhs1)
        }

        def checkPruning(u: State, v: State): Unit = {
            val (lhs0, rhs0) = fixedPoint[State](linEqRule, u)
            val (lhs1, rhs1) = v
            for ((d0, d1) <- lhs0.view.zip(lhs1)) {
                assertEq(d0, d1)
            }
            assertEq(rhs0, rhs1)
        }

        // propagate from rhs to lhs: enforce disjunction
        checkPruning(
            (List(FalseDomain), TrueDomain),
            (List(EmptyBooleanDomain), EmptyBooleanDomain))
        checkPruning(
            (List(FalseDomain, CompleteBooleanDomain), TrueDomain),
            (List(FalseDomain, TrueDomain), TrueDomain))

        // propagate from rhs to lhs: enforce negation of disjunction
        checkPruning(
            (List(CompleteBooleanDomain), FalseDomain),
            (List(FalseDomain), FalseDomain))
        checkPruning(
            (List(TrueDomain), FalseDomain),
            (List(EmptyBooleanDomain), EmptyBooleanDomain))
        checkPruning(
            (List(FalseDomain, TrueDomain), FalseDomain),
            (List(EmptyBooleanDomain, EmptyBooleanDomain), EmptyBooleanDomain))

        // propagate from lhs to rhs
        checkPruning(
            (List(TrueDomain), CompleteBooleanDomain),
            (List(TrueDomain), TrueDomain))
        checkPruning(
            (List(FalseDomain, TrueDomain), CompleteBooleanDomain),
            (List(FalseDomain, TrueDomain), TrueDomain))
        checkPruning(
            (List(FalseDomain, FalseDomain), CompleteBooleanDomain),
            (List(FalseDomain, FalseDomain), FalseDomain))

        // empty domains
        checkPruning(
            (List(FalseDomain, TrueDomain, CompleteBooleanDomain), EmptyBooleanDomain),
            (List(EmptyBooleanDomain, EmptyBooleanDomain, EmptyBooleanDomain), EmptyBooleanDomain))
        checkPruning(
            (List(FalseDomain, TrueDomain, EmptyBooleanDomain), FalseDomain),
            (List(EmptyBooleanDomain, EmptyBooleanDomain, EmptyBooleanDomain), EmptyBooleanDomain))

    }

}
