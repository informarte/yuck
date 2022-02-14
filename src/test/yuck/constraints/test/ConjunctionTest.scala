package yuck.constraints.test

import org.junit.*

import yuck.constraints.*
import yuck.constraints.test.util.ConstraintTestTooling
import yuck.core.*
import yuck.test.util.UnitTest

/**
 * @author Michael Marte
 *
 */
@FixMethodOrder(runners.MethodSorters.NAME_ASCENDING)
final class ConjunctionTest extends UnitTest with ConstraintTestTooling {

    private val space = new Space(logger, sigint)

    private val baseDomain = CompleteBooleanDecisionDomain
    private val xs = for (i <- 1 to 3) yield new BooleanVariable(space.nextVariableId, "x%d".format(i), baseDomain)
    private val Seq(x1, x2, x3) = xs
    private val y = new BooleanVariable(space.nextVariableId, "y", baseDomain)
    private val constraint = new Conjunction(space.nextConstraintId, null, xs, y)

    @Test
    def testBasics: Unit = {
        assertEq(constraint.toString, "y = and([x1, x2, x3])")
        assertEq(constraint.inVariables.size, xs.size)
        assertEq(constraint.inVariables.toSet, xs.toSet)
        assertEq(constraint.outVariables.size, 1)
        assertEq(constraint.outVariables.head, y)
    }

    @Test
    def testPropagation: Unit = {
        space.post(constraint)
        runScenario(
            TestScenario(
                space,
                Propagate("root-node propagation", Nil, Nil),
                PropagateAndRollback("enforce constraint 1", List(y << TrueDomain), xs.map(x => x << TrueDomain)),
                PropagateAndRollback(
                    "enforce constraint 2", List(x1 << FalseDomain, y << TrueDomain), List(x1 << EmptyBooleanDomain)),
                PropagateAndRollback(
                    "enforce negation 1", List(x1 << FalseDomain, x2 << TrueDomain, y << FalseDomain), Nil),
                PropagateAndRollback(
                    "enforce negation 2", List(x1 << TrueDomain, x2 << TrueDomain, y << FalseDomain), List(x3 << FalseDomain)),
                PropagateAndRollback("lhs to rhs 1", List(x1 << FalseDomain), List(y << FalseDomain)),
                PropagateAndRollback("lhs to rhs 2", List(x1 << TrueDomain), Nil),
                PropagateAndRollback(
                    "lhs to rhs 3", List(x1 << TrueDomain, x2 << TrueDomain, x3 << TrueDomain), List(y << TrueDomain))))
    }

    @Test
    def testComputationOfViolation: Unit = {
        space.post(constraint)
        runScenario(
            TestScenario(
                space,
                Initialize("1", x1 << True, x2 << True, x3 << True, y << True),
                Initialize("2", x1 << False, x2 << False2, x3 << False3, y << False6),
                Consult("1", x2 << False3, y << False7),
                Consult("2", x1 << False3, x3 << False, y << False6),
                ConsultAndCommit("1", x2 << False3, y << False7),
                ConsultAndCommit("2", x1 << True, x3 << False2, y << False5)))
    }

}
