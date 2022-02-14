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
final class LinearCombinationTest extends UnitTest with ConstraintTestTooling {

    private val space = new Space(logger, sigint)

    private val baseDomain = IntegerRange(0, 9)
    private val xs = for (i <- 1 to 3) yield new IntegerVariable(space.nextVariableId, "x%d".format(i), baseDomain)
    private val Seq(x1, x2, x3) = xs
    private val y = new IntegerVariable(space.nextVariableId, "y", CompleteIntegerRange)
    private val axs = List(AX(Two, x1), AX(Zero, x2), AX(One, x3))
    private val constraint = new LinearCombination(space.nextConstraintId, null, axs, y)

    @Test
    def testBasics: Unit = {
        assertEq(constraint.toString, "y = sum([2 * x1, 0 * x2, 1 * x3])")
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
                Propagate("root-node propagation", Nil, List(y << (0, 27))),
                PropagateAndRollback("1", List(x1 << (2, 7), x2 << (3, 6), x3 << (4, 5)), List(y << (8, 19))),
                PropagateAndRollback("2", List(y << (0, 7)), List(x1 << (0, 3), x3 << (0, 7))),
                PropagateAndRollback("3", List(x1 << (2, 9), y << (0, 7)), List(x1 << (2, 3), x3 << (0, 3), y << (4, 7))),
                PropagateAndRollback("4", List(x3 << (0, 1), y << (4, 9)), List(x1 << (2, 4)))))
    }

    @Test
    def testSumComputation: Unit = {
        space.post(constraint)
        runScenario(
            TestScenario(
                space,
                Initialize("1", x1 << 3, x2 << 2, x3 << 1, y << 7),
                Initialize("2", x1 << 1, x2 << 2, x3 << 3, y << 5),
                Consult("1", x1 << 3, y << 9),
                Consult("2", x1 << 2, x2 << 3, y << 7),
                ConsultAndCommit("1", x1 << 3, y << 9),
                ConsultAndCommit("2", x2 << 3, x3 << 1, y << 7)))
    }

}
