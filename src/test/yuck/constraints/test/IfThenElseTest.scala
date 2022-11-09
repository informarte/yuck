package yuck.constraints.test

import org.junit.*

import yuck.constraints.IfThenElse
import yuck.constraints.test.util.ConstraintTestTooling
import yuck.core.{given, *}
import yuck.test.util.UnitTest

/**
 * @author Michael Marte
 *
 */
@FixMethodOrder(runners.MethodSorters.NAME_ASCENDING)
final class IfThenElseTest extends UnitTest with ConstraintTestTooling {

    private val space = new Space(logger, sigint)

    private val cs = for (i <- 1 to 3) yield new BooleanVariable(space.nextVariableId(), "c%d".format(i), CompleteBooleanDomain)
    private val Seq(c1, c2, c3) = cs
    private val xs = for (i <- 1 to 3) yield new IntegerVariable(space.nextVariableId(), "x%d".format(i), CompleteIntegerRange)
    private val Seq(x1, x2, x3) = xs
    private val y = new IntegerVariable(space.nextVariableId(), "y", CompleteIntegerRange)
    private val constraint = new IfThenElse(space.nextConstraintId(), None, cs, xs, y)

    @Test
    def testBasics(): Unit = {
        assertEq(constraint.toString, "if_then_else([%s], [%s], %s)".format(cs.mkString(", "), xs.mkString(", "), y))
        assertEq(constraint.inVariables.size, cs.size + xs.size)
        assertEq(constraint.inVariables.toSet, (cs ++ xs).toSet)
        assertEq(constraint.outVariables.size, 1)
        assertEq(constraint.outVariables.head, y)
    }

    @Test
    def testPropagation(): Unit = {
        space.post(constraint)
        runScenario(
            TestScenario(
                space,
                PropagateAndRollback("no restrictions, so no propagation", Nil, Nil),
                PropagateAndRollback(
                    "propagate the union of the x[i] domains to y",
                    List(x1 << PositiveIntegerRange, x2 << ZeroToZeroIntegerRange, x3 << PositiveIntegerRange),
                    List(c1 << CompleteBooleanDomain, c2 << CompleteBooleanDomain,
                         c3 << CompleteBooleanDomain,
                         x1 << PositiveIntegerRange, x2 << ZeroToZeroIntegerRange, x3 << PositiveIntegerRange,
                         y << NonNegativeIntegerRange)),
                PropagateAndRollback(
                    "if c1 = true, y = x1",
                    List(c1 << TrueDomain, x1 << NonPositiveIntegerRange, y << NonNegativeIntegerRange),
                    List(c2 << CompleteBooleanDomain, c3 << CompleteBooleanDomain,
                         x1 << ZeroToZeroIntegerRange, x2 << CompleteIntegerRange, x3 << CompleteIntegerRange,
                         y << ZeroToZeroIntegerRange)),
                PropagateAndRollback(
                    "if c1 = false, d(x1) plays no role",
                    List(c1 << FalseDomain,
                         x1 << PositiveIntegerRange, x2 << NegativeIntegerRange, x3 << NegativeIntegerRange),
                    List(c2 << CompleteBooleanDomain, c3 << CompleteBooleanDomain,
                         x1 << PositiveIntegerRange, x2 << NegativeIntegerRange, x3 << NegativeIntegerRange,
                         y << NegativeIntegerRange)),
                PropagateAndRollback(
                    "if c2 = false, d(x2) plays no role",
                    List(c2 << FalseDomain,
                         x1 << NegativeIntegerRange, x2 << PositiveIntegerRange, x3 << NegativeIntegerRange),
                    List(c1 << CompleteBooleanDomain, c3 << CompleteBooleanDomain,
                         x1 << NegativeIntegerRange, x2 << PositiveIntegerRange, x3 << NegativeIntegerRange,
                         y << NegativeIntegerRange)),
                PropagateAndRollback(
                    "if c1 = false and c2 = false, d(x1) and d(x2) play no role and y = x3",
                    List(c1 << FalseDomain, c2 << FalseDomain, c3 << TrueDomain,
                         x1 << PositiveIntegerRange, x2 << PositiveIntegerRange, x3 << NonPositiveIntegerRange,
                         y << NonNegativeIntegerRange),
                    List(x1 << PositiveIntegerRange, x2 << PositiveIntegerRange, x3 << ZeroToZeroIntegerRange,
                         y << ZeroToZeroIntegerRange)),
                PropagateAndRollback(
                    "if d(x1) and d(y) are disjoint, c1 = false",
                    List(x1 << PositiveIntegerRange, y << NegativeIntegerRange),
                    List(c1 << FalseDomain, c2 << CompleteBooleanDomain, c3 << CompleteBooleanDomain,
                         x1 << PositiveIntegerRange, x2 << CompleteIntegerRange, x3 << CompleteIntegerRange,
                         y << NegativeIntegerRange)),
                PropagateAndRollback(
                    "if both d(x2) and d(x3) are disjoint from d(y), c1 = true and y = x1",
                    List(x1 << NonNegativeIntegerRange, x2 << PositiveIntegerRange, x3 << PositiveIntegerRange,
                         y << NonPositiveIntegerRange),
                    List(c1 << TrueDomain, c2 << CompleteBooleanDomain, c3 << CompleteBooleanDomain,
                         x1 << ZeroToZeroIntegerRange, x2 << PositiveIntegerRange, x3 << PositiveIntegerRange,
                         y << ZeroToZeroIntegerRange)),
                PropagateAndRollback(
                    "if both d(x1) and d(x3) are disjoint from d(y), c1 = false, c2 = true and y = x2",
                    List(x1 << PositiveIntegerRange, x2 << NonNegativeIntegerRange, x3 << PositiveIntegerRange,
                         y << NonPositiveIntegerRange),
                    List(c1 << FalseDomain, c2 << TrueDomain, c3 << CompleteBooleanDomain,
                         x1 << PositiveIntegerRange, x2 << ZeroToZeroIntegerRange, x3 << PositiveIntegerRange,
                         y << ZeroToZeroIntegerRange)),
                PropagateAndRollback(
                    "if both d(x1) and d(x2) are disjoint from d(y), c1 = false, c2 = false, c3 = true and y = x3",
                    List(x1 << PositiveIntegerRange, x2 << PositiveIntegerRange, x3 << NonNegativeIntegerRange,
                         y << NonPositiveIntegerRange),
                    List(c1 << FalseDomain, c2 << FalseDomain, c3 << TrueDomain,
                         x1 << PositiveIntegerRange, x2 << PositiveIntegerRange, x3 << ZeroToZeroIntegerRange,
                         y << ZeroToZeroIntegerRange)),
                PropagateAndRollback(
                    "if all d(x[i]) are disjoint from d[y], then there is no solution",
                    List(x1 << PositiveIntegerRange, x2 << PositiveIntegerRange, x3 << PositiveIntegerRange,
                         y << NonPositiveIntegerRange),
                    List(c1 << EmptyBooleanDomain))))
    }

    @Test
    def testCostComputation(): Unit = {
        space.post(constraint)
        runScenario(
            TestScenario(
                space,
                Initialize("select 1st case", c1 << True, c2 << False, c3 << True, x1 << 1, x2 << 2, x3 << 3, y << 1),
                Initialize("select 2nd case", c1 << False, c2 << True, c3 << True, x1 << 1, x2 << 2, x3 << 3, y << 2),
                Initialize("select else case", c1 << False, c2 << False, c3 << True, x1 << 1, x2 << 2, x3 << 3, y << 3),
                Consult("select 1st case", c1 << True, y << 1),
                Consult("change result of else case", x3 << 4, y << 4),
                Consult("select 2nd case and change its result", c2 << True, x2 << 4, y << 4),
                ConsultAndCommit("select 1st case", c1 << True, y << 1),
                ConsultAndCommit("change result of 2nd case", x2 << 4, y << 1),
                ConsultAndCommit("select 2nd case", c1 << False, c2 << True, y << 4)))
    }

}
