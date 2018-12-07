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
        (prune: (BooleanDomain, BooleanDomain) => (BooleanDomain, BooleanDomain),
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
    def testEqPruning {
        testPruning(BooleanDomainPruner.eq, (a, b) => a.truthValue == b.truthValue)
    }

    @Test
    def testNePruning {
        testPruning(BooleanDomainPruner.ne, (a, b) => a.truthValue != b.truthValue)
    }

    @Test
    def testLePruning {
        testPruning(BooleanDomainPruner.le, (a, b) => ! a.truthValue || b.truthValue)
    }

    @Test
    def testLtPruning {
        testPruning(BooleanDomainPruner.lt, (a, b) => ! a.truthValue && b.truthValue)
    }

    @Test
    def testLinEqPruning {

        type LinearCombination = List[(BooleanValue, BooleanDomain)]
        type State = (LinearCombination, BooleanDomain)

        def linEq(u: State): State = {
            val (lhs0, rhs0) = u
            val (lhs1, rhs1) = BooleanDomainPruner.linEq(lhs0, rhs0)
            (lhs0.toIterator.map(_._1).zip(lhs1.toIterator).toList, rhs1)
        }

        def checkPruning(u: State, v: State) {
            assertEq(fixedPoint[State](linEq, u), v)
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
