package yuck.constraints.test

import org.junit.*

import yuck.constraints.AlldistinctExcept
import yuck.constraints.test.util.ConstraintTestTooling
import yuck.core.{given, *}
import yuck.test.*
import yuck.test.util.UnitTest

/**
 * @author Michael Marte
 *
 */
@FixMethodOrder(runners.MethodSorters.NAME_ASCENDING)
final class AlldistinctExceptTest extends UnitTest with ConstraintTestTooling {

    private val space = new Space(logger, sigint)

    private val xs = for (i <- 1 to 4) yield new IntegerVariable(space.nextVariableId(), "x%d".format(i), IntegerRange(0, 9))
    private val Seq(x1, x2, x3, x4) = xs
    private val costs = new BooleanVariable(space.nextVariableId(), "costs", CompleteBooleanDomain)

    @Test
    def testBasics(): Unit = {
        val constraint = new AlldistinctExcept(space.nextConstraintId(), null, xs, Set(Zero), costs)
        assertEq(constraint.toString, "alldistinctExcept([x1, x2, x3, x4], {0}, costs)")
        assertEq(constraint.inVariables.size, 4)
        assertEq(constraint.inVariables.toSet, xs.toSet)
        assertEq(constraint.outVariables.size, 1)
        assertEq(constraint.outVariables.head, costs)
    }

    @Test
    def testCostComputation(): Unit = {
        space.post(new AlldistinctExcept(space.nextConstraintId(), null, xs, Set(Zero, Nine), costs))
        runScenario(
            TestScenario(
                space,
                Initialize("no conflict", x1 << 1, x2 << 2, x3 << 3, x4 << 4, costs << True),
                Initialize("no conflict (with irrelevant values)", x1 << 0, x2 << 0, x3 << 9, x4 << 9, costs << True),
                Initialize("one conflict", x1 << 1, x2 << 1, x3 << 3, x4 << 4, costs << False),
                Initialize("two conflicts", x1 << 1, x2 << 1, x3 << 3, x4 << 3, costs << False2),
                Consult("fix a conflict", x2 << 2, costs << False),
                Consult("fix both conflicts", x2 << 2, x4 << 4, costs << True),
                Consult("fix both conflicts (with irrelevant values)", x1 << 0, x3 << 9, costs << True),
                ConsultAndCommit("fix both conflicts", x2 << 2, x4 << 4, costs << True),
                ConsultAndCommit("cause three conflicts", x1 << 1, x2 << 1, x3 << 1, x4 << 1, costs << False3),
                ConsultAndCommit("fix both conflicts (with irrelevant values)", x1 << 0, x2 << 0, x3 << 9, x4 << 9, costs << True)))
    }

    @Test
    def testCostComputationWithAVariableOccurringTwice(): Unit = {
        space.post(new AlldistinctExcept(space.nextConstraintId(), null, List(x1, x2, x2), Set(Zero), costs))
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
