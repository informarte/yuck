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
final class CountVarTest extends UnitTest with ConstraintTestTooling {

    private val space = new Space(logger, sigint)

    private val baseDomain = IntegerRange(0, 9)
    private val xs = for (i <- 1 to 3) yield new IntegerVariable(space.nextVariableId, "x%d".format(i), baseDomain)
    private val Seq(x1, x2, x3) = xs
    private val y = new IntegerVariable(space.nextVariableId, "y", baseDomain)
    private val n = new IntegerVariable(space.nextVariableId, "n", NonNegativeIntegerRange)

    @Test
    def testBasics: Unit = {
        val constraint = new CountVar(space.nextConstraintId, null, xs, y, n)
        assertEq(constraint.toString, "n = count(y, [x1, x2, x3])")
        assertEq(constraint.inVariables.size, xs.size + 1)
        assertEq(constraint.inVariables.toSet, Set(y).concat(xs))
        assertEq(constraint.outVariables.size, 1)
        assertEq(constraint.outVariables.head, n)
    }

    @Test
    def testCounting: Unit = {
        space.post(new CountVar(space.nextConstraintId, null, xs, y, n))
        runScenario(
            TestScenario(
                space,
                Initialize("1", x1 << 2, x2 << 3, x3 << 3, y << 1, n << 0),
                Initialize("2", x1 << 1, x2 << 1, x3 << 1, y << 1, n << 3),
                Consult("1", x1 << 2, n << 2),
                Consult("2", x1 << 2, y << 2, n << 1),
                ConsultAndCommit("1", x1 << 2, n << 2),
                ConsultAndCommit("2", x1 << 1, x3 << 2, n << 2),
                ConsultAndCommit("3", y << 2, n << 1),
                ConsultAndCommit("4", y << 1, x3 << 1, n << 3)))
    }

    @Test
    def testHandlingOfDuplicateVariablesInCounting: Unit = {
        space.post(new CountVar(space.nextConstraintId, null, List(x1, x1, x2, x3), y, n))
        runScenario(
            TestScenario(
                space,
                Initialize("1", x1 << 2, x2 << 3, x3 << 3, y << 1, n << 0),
                Initialize("2", x1 << 1, x2 << 1, x3 << 1, y << 1, n << 4),
                Consult("1", x1 << 2, n << 2),
                Consult("2", x1 << 2, y << 2, n << 2),
                ConsultAndCommit("1", x1 << 2, n << 2),
                ConsultAndCommit("2", x1 << 1, x3 << 2, n << 3),
                ConsultAndCommit("3", y << 2, n << 1),
                ConsultAndCommit("4", y << 1, x3 << 1, n << 4)))
    }

}
