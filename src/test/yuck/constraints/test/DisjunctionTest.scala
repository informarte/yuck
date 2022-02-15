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
@Test
@FixMethodOrder(runners.MethodSorters.NAME_ASCENDING)
final class DisjunctionTest extends UnitTest with ConstraintTestTooling {

    private val space = new Space(logger, sigint)

    private val baseDomain = CompleteBooleanDecisionDomain
    private val xs = for (i <- 1 to 3) yield new BooleanVariable(space.nextVariableId, "x%d".format(i), baseDomain)
    private val Seq(x1, x2, x3) = xs
    private val y = new BooleanVariable(space.nextVariableId, "y", baseDomain)
    private val constraint = new Disjunction(space.nextConstraintId, null, xs, y)

    @Test
    def testBasics(): Unit = {
        assertEq(constraint.toString, "y = or([x1, x2, x3])")
        assertEq(constraint.inVariables.size, xs.size)
        assertEq(constraint.inVariables.toSet, xs.toSet)
        assertEq(constraint.outVariables.size, 1)
        assertEq(constraint.outVariables.head, y)
    }

    @Test
    def testPropagation(): Unit = {
        space.post(constraint)
        runScenario(
            TestScenario(
                space,
                Propagate("root-node propagation", Nil, Nil),
                PropagateAndRollback("enforce constraint 1", List(y << TrueDomain), Nil),
                PropagateAndRollback(
                    "enforce constraint 2", List(x1 << FalseDomain, x2 << FalseDomain, y << TrueDomain), List(x3 << TrueDomain)),
                PropagateAndRollback(
                    "enforce negation 1", List(y << FalseDomain), List(x1 << FalseDomain, x2 << FalseDomain, x3 << FalseDomain)),
                PropagateAndRollback(
                    "enforce negation 2", List(x1 << TrueDomain, y << FalseDomain), List(x1 << EmptyBooleanDomain)),
                PropagateAndRollback("lhs to rhs 1", List(x1 << TrueDomain), List(y << TrueDomain)),
                PropagateAndRollback("lhs to rhs 2", List(x1 << FalseDomain, x2 << FalseDomain), Nil),
                PropagateAndRollback(
                    "lhs to rhs 3", List(x1 << TrueDomain, x2 << TrueDomain, x3 << TrueDomain), List(y << TrueDomain))))
    }

    @Test
    def testComputationOfViolation(): Unit = {
        space.post(constraint)
        runScenario(
            TestScenario(
                space,
                Initialize("1", x1 << False, x2 << True, x3 << False3, y << True),
                Initialize("2", x1 << False, x2 << False2, x3 << False3, y << False2),
                Consult("1", x3 << False2, y << False),
                Consult("2", x1 << True, y << True),
                Consult("3", x1 << False3, x2 << False3, y << False3),
                Consult("4", x1 << False3, x2 << True, y << True),
                ConsultAndCommit("1", x3 << False2, y << False),
                ConsultAndCommit("2", x1 << True, y << True),
                ConsultAndCommit("3", x1 << False3, x2 << False3, x3 << False3, y << False3),
                ConsultAndCommit("4", x1 << False2, x2 << True, y << True)))
    }

}
