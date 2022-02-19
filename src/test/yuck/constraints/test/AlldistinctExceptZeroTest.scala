package yuck.constraints.test

import org.junit.*

import yuck.constraints.AlldistinctExceptZero
import yuck.constraints.test.util.ConstraintTestTooling
import yuck.core.*
import yuck.test.util.UnitTest

/**
 * @author Michael Marte
 *
 */
@FixMethodOrder(runners.MethodSorters.NAME_ASCENDING)
final class AlldistinctExceptZeroTest extends UnitTest with ConstraintTestTooling {

    private val space = new Space(logger, sigint)

    private val baseDomain = IntegerRange(0, 9)
    private val xs = for (i <- 1 to 3) yield new IntegerVariable(space.nextVariableId(), "x%d".format(i), baseDomain)
    private val Seq(x1, x2, x3) = xs
    private val costs = new BooleanVariable(space.nextVariableId(), "costs", CompleteBooleanDomain)

    @Test
    def testBasics(): Unit = {
        val constraint = new AlldistinctExceptZero(space.nextConstraintId(), null, xs, costs)
        assertEq(constraint.toString, "alldistinctExceptZero([x1, x2, x3], costs)")
        assertEq(constraint.inVariables.size, 3)
        assertEq(constraint.inVariables.toSet, xs.toSet)
        assertEq(constraint.outVariables.size, 1)
        assertEq(constraint.outVariables.head, costs)
    }

    @Test
    def testCostComputation(): Unit = {
        space.post(new AlldistinctExceptZero(space.nextConstraintId(), null, xs, costs))
        runScenario(
            TestScenario(
                space,
                Initialize("no conflict", x1 << 1, x2 << 2, x3 << 3, costs << True),
                Initialize("no conflict (with zeros)", x1 << 0, x2 << 0, x3 << 3, costs << True),
                Initialize("one conflict", x1 << 1, x2 << 1, x3 << 3, costs << False),
                Initialize("two conflicts", x1 << 1, x2 << 1, x3 << 1, costs << False2),
                Consult("fix a conflict", x3 << 3, costs << False),
                Consult("fix both conflicts", x2 << 2, x3 << 3, costs << True),
                Consult("fix both conflicts (with zeros)", x1 << 0, x2 << 0, costs << True),
                ConsultAndCommit("fix both conflicts", x2 << 2, x3 << 3, costs << True),
                ConsultAndCommit("cause two conflicts", x1 << 1, x2 << 1, x3 << 1, costs << False2),
                ConsultAndCommit("fix both conflicts (with zeros)", x1 << 0, x2 << 0, costs << True)))
    }

    @Test
    def testCostComputationWithAVariableOccurringTwice(): Unit = {
        space.post(new AlldistinctExceptZero(space.nextConstraintId(), null, List(x1, x2, x2), costs))
        runScenario(
            TestScenario(
                space,
                Initialize("no conflict (with zeros)", x1 << 1, x2 << 0, costs << True),
                Initialize("one conflict", x1 << 1, x2 << 2, costs << False),
                Initialize("two conflicts", x1 << 1, x2 << 1, costs << False2),
                Consult("fix a conflict", x2 << 2, costs << False),
                Consult("fix both conflicts (with zeros)", x2 << 0, costs << True),
                ConsultAndCommit("fix a conflict", x2 << 2, costs << False),
                ConsultAndCommit("fix both conflicts (with zeros)", x2 << 0, costs << True)))
    }

}
