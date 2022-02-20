package yuck.constraints.test

import org.junit.*

import yuck.constraints.LexLessEq
import yuck.constraints.test.util.ConstraintTestTooling
import yuck.core.*
import yuck.test.util.UnitTest

/**
 * @author Michael Marte
 *
 */
@FixMethodOrder(runners.MethodSorters.NAME_ASCENDING)
final class LexLessEqTest extends UnitTest with ConstraintTestTooling {

    private val space = new Space(logger, sigint)

    private val baseDomain = CompleteIntegerRange
    private val xs = for (i <- 1 to 3) yield new IntegerVariable(space.nextVariableId, "x%d".format(i), baseDomain)
    private val Seq(x1, x2, x3) = xs
    private val ys = for (i <- 1 to 3) yield new IntegerVariable(space.nextVariableId, "y%d".format(i), baseDomain)
    private val Seq(y1, y2, y3) = ys
    private val costs = new BooleanVariable(space.nextVariableId, "costs", CompleteBooleanDomain)

    @Test
    def testBasics(): Unit = {
        val constraint = new LexLessEq(space.nextConstraintId, null, xs, ys, costs)
        assertEq(constraint.toString, "lex_lesseq([x1, x2, x3], [y1, y2, y3])")
        assertEq(constraint.inVariables.size, 6)
        assertEq(constraint.inVariables.toSet, xs.toSet.union(ys.toSet))
        assertEq(constraint.outVariables.size, 1)
        assertEq(constraint.outVariables.head, costs)
    }

    @Test
    def testCostComputation23(): Unit = {
        space.post(new LexLessEq(space.nextConstraintId, null, Vector(x1, x2), ys, costs))
        runScenario(
            TestScenario(
                space,
                Initialize("setup", x1 << 1, x2 << 1, y1 << 1, y2 << 1, y3 << 1, costs << True),
                ConsultAndCommit("1", x1 << 0, x2 << 0, costs << True),
                ConsultAndCommit("2", y1 << 0, y2 << 0, y3 << 0, costs << True),
                ConsultAndCommit("3", x2 << 1, costs << False),
                ConsultAndCommit("4", x1 << 1, costs << False2)))
    }

    @Test
    def testCostComputation32(): Unit = {
        space.post(new LexLessEq(space.nextConstraintId, null, xs, Vector(y1, y2), costs))
        runScenario(
            TestScenario(
                space,
                Initialize("setup", x1 << 1, x2 << 1, x3 << 1, y1 << 1, y2 << 1, costs << False),
                ConsultAndCommit("1", x1 << 0, x2 << 0, x3 << 0, costs << True),
                ConsultAndCommit("2", y1 << 0, y2 << 0, costs << False),
                ConsultAndCommit("3", x3 << 1, costs << False),
                ConsultAndCommit("4", x2 << 1, costs << False),
                ConsultAndCommit("5", x1 << 1, costs << False2)))
    }

    @Test
    def testCostComputation33(): Unit = {
        space.post(new LexLessEq(space.nextConstraintId, null, xs, ys, costs))
        runScenario(
            TestScenario(
                space,
                Initialize("setup", x1 << 1, x2 << 1, x3 << 1, y1 << 1, y2 << 1, y3 << 1, costs << True),
                ConsultAndCommit("1", x1 << 0, x2 << 0, x3 << 0, costs << True),
                ConsultAndCommit("2", y1 << 0, y2 << 0, y3 << 0, costs << True),
                ConsultAndCommit("3", x3 << 1, costs << False),
                ConsultAndCommit("4", x2 << 1, costs << False2),
                ConsultAndCommit("5", x1 << 1, costs << False3)))
    }

}