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
final class RegularTest extends UnitTest with ConstraintTestTooling {

    private val space = new Space(logger, sigint)

    private val baseDomain = IntegerRange(1, 2)
    private val xs = for (i <- 1 to 10) yield new IntegerVariable(space.nextVariableId, "x%d".format(i), baseDomain)
    private val Seq(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10) = xs
    private val Q = 6
    private val S = 2
    private val delta = Vector(1, 2, 3, 0, 3, 4, 0, 5, 0, 6, 6, 0).grouped(2).toVector
    private val q0 = 1
    private val F = IntegerRange(6, 6)
    private val costs = new BooleanVariable(space.nextVariableId, "costs", CompleteBooleanDomain)

    @Test
    def testBasics(): Unit = {
        val constraint = new Regular(space.nextConstraintId, null, xs, Q, S, delta, q0, F, costs)
        assertEq(
            constraint.toString,
            "regular([%s], 6, 2, [[1, 2], [3, 0], [3, 4], [0, 5], [0, 6], [6, 0]], 1, {6}, costs)"
                .format(xs.mkString(", ")))
        assertEq(constraint.inVariables.size, xs.size)
        assertEq(constraint.inVariables.toSet, xs.toSet)
        assertEq(constraint.outVariables.size, 1)
        assertEq(constraint.outVariables.head, costs)
    }

    @Test
    def testCostComputation(): Unit = {
        space.post(new Regular(space.nextConstraintId, null, xs, Q, S, delta, q0, F, costs))
        runScenario(
            TestScenario(
                space,
                // input:  2, 1, 1, 2, 2, 2, 2, 2, 1, 2
                // states: 2, 3, 3, 4, 5, 6, 0, 0, 0, 0
                Initialize(
                    "invalid sequence",
                    x1 << 2, x2 << 1, x3 << 1, x4 << 2, x5 << 2, x6 << 2, x7 << 2, x8 << 2, x9 << 1, x10 << 2,
                    costs << False4),
                // input:  1, 1, 1, 2, 1, 2, 2, 2, 1, 1
                // states: 1, 1, 1, 2, 3, 4, 5, 6, 6, 6
                Initialize(
                    "valid sequence",
                    x1 << 1, x2 << 1, x3 << 1, x4 << 2, x5 << 1, x6 << 2, x7 << 2, x8 << 2, x9 << 1, x10 << 1,
                    costs << True),
                // input:  1, 1, 1, 2, 1, 0, 2, 2, 1, 1
                // states: 1, 1, 1, 2, 3, 0, 0, 0, 0, 0
                Consult("1", x6 << 0, costs << False5),
                // input:  1, 1, 2, 2, 1, 2, 2, 2, 2, 1
                // states: 1, 1, 2, 0, 0, 0, 0, 0, 0, 0
                Consult("2", x3 << 2, costs << False7),
                // input:  1, 1, 1, 2, 2, 2, 2, 2, 1, 1
                // states: 1, 1, 1, 2, 0, 0, 0, 0, 0, 0
                ConsultAndCommit("1", x5 << 2, costs << False6),
                // input:  1, 1, 1, 2, 1, 2, 2, 2, 1, 2
                // states: 1, 1, 1, 2, 3, 4, 5, 6, 6, 0
                ConsultAndCommit("2", x5 << 1, x10 << 2, costs << False),
                // input:  1, 1, 1, 2, 1, 2, 2, 2, 1, 1
                // states: 1, 1, 1, 2, 3, 4, 5, 6, 6, 6
                ConsultAndCommit("3", x10 << 1, costs << True)))
    }

    @Test
    def testHandlingOfInvalidInputsInCostComputation(): Unit = {
        space.post(new Regular(space.nextConstraintId, null, xs, Q, S, delta, q0, F, costs))
        runScenario(
            TestScenario(
                space,
                // input:  1, 1, 1, 2, 1, 0, 2, 2, 1, 1
                // states: 1, 1, 1, 2, 3, 0, 0, 0, 0, 0
                Initialize(
                    "invalid input",
                    x1 << 1, x2 << 1, x3 << 1, x4 << 2, x5 << 1, x6 << 0, x7 << 2, x8 << 2, x9 << 1, x10 << 1,
                    costs << False5),
                // input:  1, 1, 1, 2, 1, 2, 2, 2, 1, 1
                // states: 1, 1, 1, 2, 3, 4, 5, 6, 6, 6
                ConsultAndCommit("fix sequence", x6 << 2, costs << True)))
    }

    @Test
    def testHandlingOfDuplicateVariablesInCostComputation(): Unit = {
        val xs1 = xs.updated(4, x1).updated(9, x1)
        space.post(new Regular(space.nextConstraintId, null, xs1, Q, S, delta, q0, F, costs))
        runScenario(
            TestScenario(
                space,
                // input:  2, 1, 1, 2, 2, 2, 2, 2, 1, 2
                // states: 2, 3, 3, 4, 5, 6, 0, 0, 0, 0
                Initialize(
                    "invalid sequence",
                    x1 << 2, x2 << 1, x3 << 1, x4 << 2, x6 << 2, x7 << 2, x8 << 2, x9 << 1,
                    costs << False4),
                // input:  1, 1, 1, 2, 1, 2, 2, 2, 1, 1
                // states: 1, 1, 1, 2, 3, 4, 5, 6, 6, 6
                ConsultAndCommit("fix sequence", x1 << 1, costs << True)
            ))
    }

}
