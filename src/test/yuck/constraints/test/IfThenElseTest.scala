package yuck.constraints.test

import org.junit._

import scala.collection.IndexedSeq

import yuck.constraints.IfThenElse
import yuck.core._
import yuck.test.util.UnitTest

/**
 * @author Michael Marte
 *
 */
@FixMethodOrder(runners.MethodSorters.NAME_ASCENDING)
final class IfThenElseTest
    extends UnitTest
    with AssignmentPropagationTestTooling
    with DomainPropagationTestTooling
{

    private val space = new Space(logger, sigint)
    private val cs @ IndexedSeq(c1, c2, c3) =
        for (i <- 1 to 3) yield new BooleanVariable(space.nextVariableId, "c%d".format(i), CompleteBooleanDecisionDomain)
    private val xs @ IndexedSeq(x1, x2, x3) =
        for (i <- 1 to 3) yield new IntegerVariable(space.nextVariableId, "x%d".format(i), CompleteIntegerRange)
    private val y = new IntegerVariable(space.nextVariableId, "y", CompleteIntegerRange)
    private val constraint = new IfThenElse(space.nextConstraintId, None, cs, xs, y)

    @Test
    def testBasics: Unit = {
        assertEq(constraint.toString, "if_then_else([%s], [%s], %s)".format(cs.mkString(", "), xs.mkString(", "), y))
        assertEq(constraint.inVariables.size, cs.size + xs.size)
        assertEq(constraint.inVariables.toSet, (cs ++ xs).toSet)
        assertEq(constraint.outVariables.size, 1)
        assertEq(constraint.outVariables.head, y)
    }

    @Test
    def testPropagation: Unit = {
        space.post(constraint)
        runScenario(
            TestScenario(
                space,
                PropagateAndRollback("no restrictions, so no propagation", Nil, () => {
                    assert(cs.forall(! _.domain.isSingleton))
                    assert(xs.forall(_.domain.isComplete))
                    assert(y.domain.isComplete)
                }),
                PropagateAndRollback(
                    "propagate the union of the x[i] domains to y",
                    List((x1, PositiveIntegerRange), (x2, ZeroToZeroIntegerRange), (x3, PositiveIntegerRange)),
                    List[AnyDomainReduction]
                        ((c1, CompleteBooleanDecisionDomain), (c2, CompleteBooleanDecisionDomain), (c3, CompleteBooleanDecisionDomain),
                         (x1, PositiveIntegerRange), (x2, ZeroToZeroIntegerRange), (x3, PositiveIntegerRange),
                         (y, NonNegativeIntegerRange))),
                PropagateAndRollback(
                    "if c[1] = true, y = x[1]",
                    List[AnyDomainReduction]
                        ((c1, TrueDomain), (x1, NonPositiveIntegerRange), (y, NonNegativeIntegerRange)),
                    List[AnyDomainReduction]
                        ((c2, CompleteBooleanDecisionDomain), (c3, CompleteBooleanDecisionDomain),
                         (x1, ZeroToZeroIntegerRange), (x2, CompleteIntegerRange), (x3, CompleteIntegerRange),
                         (y, ZeroToZeroIntegerRange))),
                PropagateAndRollback(
                    "if c[1] = false, d(x[1]) plays no role",
                    List[AnyDomainReduction]
                        ((c1, FalseDomain),
                         (x1, PositiveIntegerRange), (x2, NegativeIntegerRange), (x3, NegativeIntegerRange)),
                    List[AnyDomainReduction]
                        ((c2, CompleteBooleanDecisionDomain), (c3, CompleteBooleanDecisionDomain),
                         (x1, PositiveIntegerRange), (x2, NegativeIntegerRange), (x3, NegativeIntegerRange),
                         (y, NegativeIntegerRange))),
                PropagateAndRollback(
                    "if c[2] = false, d(x[2]) plays no role",
                    List[AnyDomainReduction]
                        ((c2, FalseDomain),
                         (x1, NegativeIntegerRange), (x2, PositiveIntegerRange), (x3, NegativeIntegerRange)),
                    List[AnyDomainReduction]
                        ((c1, CompleteBooleanDecisionDomain), (c3, CompleteBooleanDecisionDomain),
                            (x1, NegativeIntegerRange), (x2, PositiveIntegerRange), (x3, NegativeIntegerRange),
                            (y, NegativeIntegerRange))),
                PropagateAndRollback(
                    "if c[1] = false and c[2] = false, d(x[1]) and d(x[2]) play no role and y = x[3]",
                    List[AnyDomainReduction]
                        ((c1, FalseDomain), (c2, FalseDomain), (c3, TrueDomain),
                         (x1, PositiveIntegerRange), (x2, PositiveIntegerRange), (x3, NonPositiveIntegerRange),
                         (y, NonNegativeIntegerRange)),
                    List((x1, PositiveIntegerRange), (x2, PositiveIntegerRange), (x3, ZeroToZeroIntegerRange),
                         (y, ZeroToZeroIntegerRange))),
                PropagateAndRollback(
                    "if d(x[1]) and d(y) are disjoint, c[1] = false",
                    List((x1, PositiveIntegerRange), (y, NegativeIntegerRange)),
                    List[AnyDomainReduction]
                        ((c1, FalseDomain), (c2, CompleteBooleanDecisionDomain), (c3, CompleteBooleanDecisionDomain),
                         (x1, PositiveIntegerRange), (x2, CompleteIntegerRange), (x3, CompleteIntegerRange),
                         (y, NegativeIntegerRange))),
                PropagateAndRollback(
                    "if both d(x[2]) and d(x[3]) are disjoint from d(y), c[1] = true and y = x1",
                    List((x1, NonNegativeIntegerRange), (x2, PositiveIntegerRange), (x3, PositiveIntegerRange),
                        (y, NonPositiveIntegerRange)),
                    List[AnyDomainReduction]
                        ((c1, TrueDomain), (c2, CompleteBooleanDecisionDomain), (c3, CompleteBooleanDecisionDomain),
                         (x1, ZeroToZeroIntegerRange), (x2, PositiveIntegerRange), (x3, PositiveIntegerRange),
                         (y, ZeroToZeroIntegerRange))),
                PropagateAndRollback(
                    "if both d(x[1]) and d(x[3]) are disjoint from d(y), c[1] = false, c[2] = true and y = x2",
                    List((x1, PositiveIntegerRange), (x2, NonNegativeIntegerRange), (x3, PositiveIntegerRange),
                         (y, NonPositiveIntegerRange)),
                    List[AnyDomainReduction]
                        ((c1, FalseDomain), (c2, TrueDomain), (c3, CompleteBooleanDecisionDomain),
                         (x1, PositiveIntegerRange), (x2, ZeroToZeroIntegerRange), (x3, PositiveIntegerRange),
                         (y, ZeroToZeroIntegerRange))),
                PropagateAndRollback(
                    "if both d(x[1]) and d(x[2]) are disjoint from d(y), c[1] = false, c[2] = false, c[3] = true and y = x3",
                    List((x1, PositiveIntegerRange), (x2, PositiveIntegerRange), (x3, NonNegativeIntegerRange),
                        (y, NonPositiveIntegerRange)),
                    List[AnyDomainReduction]
                        ((c1, FalseDomain), (c2, FalseDomain), (c3, TrueDomain),
                         (x1, PositiveIntegerRange), (x2, PositiveIntegerRange), (x3, ZeroToZeroIntegerRange),
                         (y, ZeroToZeroIntegerRange))),
                PropagateAndRollback(
                    "if all d(x[i]) are disjoint from d[y], then there is no solution",
                    List((x1, PositiveIntegerRange), (x2, PositiveIntegerRange), (x3, PositiveIntegerRange),
                        (y, NonPositiveIntegerRange)),
                    List((c1, EmptyBooleanDomain)))))
    }

    @Test
    def testCostComputation: Unit = {
        space.post(constraint)
        runScenario(
            TestScenario(
                space,
                Initialize("select 1st case", (c1, True), (c2, False), (c3, True), (x1, One), (x2, Two), (x3, Three), (y, One)),
                Initialize("select 2nd case", (c1, False), (c2, True), (c3, True), (x1, One), (x2, Two), (x3, Three), (y, Two)),
                Initialize("select else case", (c1, False), (c2, False), (c3, True), (x1, One), (x2, Two), (x3, Three), (y, Three)),
                Consult("select 1st case", (c1, True), (y, One)),
                Consult("change result of else case", (x3, Four), (y, Four)),
                Consult("select 2nd case and change its result", (c2, True), (x2, Four), (y, Four)),
                ConsultAndCommit("select 1st case", (c1, True), (y, One)),
                ConsultAndCommit("change result of 2nd case", (x2, Four), (y, One)),
                ConsultAndCommit("select 2nd case", (c1, False), (c2, True), (y, Four))))
    }

}
