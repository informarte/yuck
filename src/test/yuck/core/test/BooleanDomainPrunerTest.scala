package yuck.core.test

import org.junit._

import yuck.core._
import yuck.util.testing.UnitTest

/**
 * @author Michael Marte
 *
 */
@FixMethodOrder(org.junit.runners.MethodSorters.NAME_ASCENDING)
class BooleanDomainPrunerTest extends UnitTest {

    import BooleanDomain.ensureDecisionDomain

    private def fixedPoint[State](f: State => State, u: State): State = {
        val v = f(u)
        if (u == v) u else fixedPoint(f, v)
    }

    private def testPruning
        [InputDomain >: BooleanDomain <: Domain[BooleanValue]]
        (prune: (InputDomain, InputDomain) => (BooleanDomain, BooleanDomain),
         predicate: (BooleanValue, BooleanValue) => Boolean): Unit =
    {
        val testData =
            List(EmptyBooleanDomain, FalseDomain, TrueDomain, CompleteBooleanDecisionDomain, CompleteBooleanDomain)
        for (d <- testData) {
            for (e <- testData) {
                val feasible =
                    ensureDecisionDomain(d).values.exists(
                        a => ensureDecisionDomain(e).values.exists(b => predicate(a, b)))
                val (f, g) = prune(d, e)
                assert(f.isSubsetOf(d))
                assert(g.isSubsetOf(e))
                assertEq(f.isEmpty || g.isEmpty, ! feasible)
                if (feasible) {
                    for (a <- ensureDecisionDomain(d).values) {
                        assertEq(ensureDecisionDomain(e).values.exists(b => predicate(a, b)), f.contains(a))
                    }
                    for (b <- ensureDecisionDomain(e).values) {
                        assertEq(ensureDecisionDomain(d).values.exists(a => predicate(a, b)), g.contains(b))
                    }
                }
            }
        }
    }

    @Test
    def testEqRule: Unit = {
        testPruning(BooleanDomainPruner.eqRule, (a, b) => a.truthValue == b.truthValue)
    }

    @Test
    def testNeRule: Unit = {
        testPruning(BooleanDomainPruner.neRule, (a, b) => a.truthValue != b.truthValue)
    }

    @Test
    def testLeRule: Unit = {
        testPruning(BooleanDomainPruner.leRule, (a, b) => ! a.truthValue || b.truthValue)
    }

    @Test
    def testLtRule: Unit = {
        testPruning(BooleanDomainPruner.ltRule, (a, b) => ! a.truthValue && b.truthValue)
    }

    @Test
    def testLinEqRule: Unit = {

        type LinearCombination = Iterable[(BooleanValue, BooleanDomain)]
        type State = (LinearCombination, BooleanDomain)

        def linEqRule(u: State): State = {
            val (lhs0, rhs0) = u
            val (lhs1, rhs1) = BooleanDomainPruner.linEqRule(lhs0, rhs0)
            (lhs0.view.map(_._1).zip(lhs1).toList, rhs1)
        }

        def checkPruning(u: State, v: State): Unit = {
            val (lhs0, rhs0) = fixedPoint[State](linEqRule, u)
            val (lhs1, rhs1) = v
            for (((_, d0), (_, d1)) <- lhs0.view.zip(lhs1)) {
                assertEq(ensureDecisionDomain(d0), ensureDecisionDomain(d1))
            }
            assertEq(ensureDecisionDomain(rhs0), ensureDecisionDomain(rhs1))
        }

        // propagate from rhs to lhs: enforce conjunction
        checkPruning(
            (List((True, FalseDomain), (False, CompleteBooleanDomain), (False, FalseDomain)), TrueDomain),
            (List((True, EmptyBooleanDomain), (False, EmptyBooleanDomain), (False, EmptyBooleanDomain)), EmptyBooleanDomain))

        // propagate from rhs to lhs: enforce negation of conjunction
        checkPruning(
            (List((True, FalseDomain), (False, TrueDomain)), FalseDomain),
            (List((True, EmptyBooleanDomain), (False, EmptyBooleanDomain)), EmptyBooleanDomain))
        checkPruning(
            (List((True, FalseDomain), (False, CompleteBooleanDomain)), FalseDomain),
            (List((True, FalseDomain), (False, FalseDomain)), FalseDomain))
        checkPruning(
            (List((True, FalseDomain), (False, CompleteBooleanDomain), (False, CompleteBooleanDomain)), FalseDomain),
            (List((True, FalseDomain), (False, CompleteBooleanDomain), (False, CompleteBooleanDomain)), FalseDomain))

        // propagate from lhs to rhs
        checkPruning(
            (List((True, FalseDomain), (False, CompleteBooleanDomain)), CompleteBooleanDomain),
            (List((True, FalseDomain), (False, CompleteBooleanDomain)), CompleteBooleanDomain))
        checkPruning(
            (List((True, FalseDomain), (False, TrueDomain)), CompleteBooleanDomain),
            (List((True, FalseDomain), (False, TrueDomain)), TrueDomain))
        checkPruning(
            (List((False, FalseDomain), (False, TrueDomain)), CompleteBooleanDomain),
            (List((False, FalseDomain), (False, TrueDomain)), FalseDomain))

        // empty domains
        checkPruning(
            (List((True, FalseDomain), (False, TrueDomain)), EmptyBooleanDomain),
            (List((True, EmptyBooleanDomain), (False, EmptyBooleanDomain)), EmptyBooleanDomain))
        checkPruning(
            (List((True, FalseDomain), (False, EmptyBooleanDomain)), FalseDomain),
            (List((True, EmptyBooleanDomain), (False, EmptyBooleanDomain)), EmptyBooleanDomain))

    }

}
