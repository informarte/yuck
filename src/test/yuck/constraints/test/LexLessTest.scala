package yuck.constraints.test

import org.junit.*

import yuck.constraints.LexLess
import yuck.constraints.test.util.ConstraintTestTooling
import yuck.core.{given, *}
import yuck.test.util.UnitTest

/**
 * @author Michael Marte
 *
 */
@FixMethodOrder(runners.MethodSorters.NAME_ASCENDING)
final class LexLessTest extends UnitTest with ConstraintTestTooling {

    private val BaseDomain = IntegerRange(0, 1)

    private val space = new Space(logger, sigint)

    private val xs = for (i <- 1 to 3) yield new IntegerVariable(space.nextVariableId(), "x%d".format(i), BaseDomain)
    private val Seq(x1, x2, x3) = xs
    private val ys = for (i <- 1 to 3) yield new IntegerVariable(space.nextVariableId(), "y%d".format(i), BaseDomain)
    private val Seq(y1, y2, y3) = ys
    private val costs = new BooleanVariable(space.nextVariableId(), "costs", CompleteBooleanDomain)

    @Test
    def testBasics(): Unit = {
        val constraint = new LexLess(space.nextConstraintId(), null, xs, ys, costs)
        assertEq(constraint.toString, "lex_less([x1, x2, x3], [y1, y2, y3])")
        assertEq(constraint.inVariables.size, 6)
        assertEq(constraint.inVariables.toSet, xs.toSet.union(ys.toSet))
        assertEq(constraint.outVariables.size, 1)
        assertEq(constraint.outVariables.head, costs)
    }

    @Test
    def testCostComputation23(): Unit = {
        space.post(new LexLess(space.nextConstraintId(), null, Vector(x1, x2), ys, costs))
        runScenario(
            TestScenario(
                space,
                Initialize("setup", x1 << 1, x2 << 1, y1 << 1, y2 << 1, y3 << 1, costs << True),
                ConsultAndCommit("1", y3 << 0, costs << True),
                ConsultAndCommit("2", y2 << 0, costs << False),
                ConsultAndCommit("3", y1 << 0, costs << False2),
                ConsultAndCommit("4", x1 << 0, costs << False),
                ConsultAndCommit("5", x2 << 0, costs << True)))
    }

    @Test
    def testCostComputation32(): Unit = {
        space.post(new LexLess(space.nextConstraintId(), null, xs, Vector(y1, y2), costs))
        runScenario(
            TestScenario(
                space,
                Initialize("setup", x1 << 1, x2 << 1, x3 << 1, y1 << 1, y2 << 1, costs << False),
                ConsultAndCommit("1", x3 << 0, costs << False),
                ConsultAndCommit("2", x2 << 0, costs << True),
                ConsultAndCommit("3", x1 << 0, costs << True),
                ConsultAndCommit("4", y1 << 0, costs << True),
                ConsultAndCommit("5", y2 << 0, costs << False)))
    }

    @Test
    def testCostComputation33(): Unit = {
        space.post(new LexLess(space.nextConstraintId(), null, xs, ys, costs))
        runScenario(
            TestScenario(
                space,
                Initialize("setup", x1 << 1, x2 << 1, x3 << 1, y1 << 1, y2 << 1, y3 << 1, costs << False),
                ConsultAndCommit("1", x1 << 0, costs << True),
                ConsultAndCommit("2", x1 << 1, x2 << 0, costs << True),
                ConsultAndCommit("3", x2 << 1, x3 << 0, costs << True),
                ConsultAndCommit("4", y3 << 0, costs << False),
                ConsultAndCommit("5", y2 << 0, costs << False2),
                ConsultAndCommit("6", y1 << 0, costs << False3)))
    }

}
