package yuck.constraints.test

import org.junit._

import yuck.constraints._
import yuck.constraints.test.util.ConstraintTestTooling
import yuck.core._
import yuck.test.util.UnitTest

/**
 * @author Michael Marte
 *
 */
@FixMethodOrder(runners.MethodSorters.NAME_ASCENDING)
final class SumTest extends UnitTest with ConstraintTestTooling {

    private val space = new Space(logger, sigint)

    private val baseDomain = IntegerRange(0, 9)
    private val xs = for (i <- 1 to 3) yield new IntegerVariable(space.nextVariableId, "x%d".format(i), baseDomain)
    private val Seq(x1, x2, x3) = xs
    private val y = new IntegerVariable(space.nextVariableId, "y", CompleteIntegerRange)
    private val constraint = new Sum(space.nextConstraintId, null, xs, y)

    @Test
    def testBasics: Unit = {
        assertEq(constraint.toString, "y = sum([x1, x2, x3])")
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
                PropagateAndRollback("1", List(x1 << (2, 7), x2 << (3, 6), x3 << (4, 5)), List(y << (9, 18))),
                PropagateAndRollback("2", List(y << (0, 7)), List(x1 << (0, 7), x2 << (0, 7), x3 << (0, 7))),
                PropagateAndRollback(
                    "3", List(x2 << (2, 9), y << (0, 7)), List(x1 << (0, 5), x2 << (2, 7), x3 << (0, 5), y << (2, 7))),
                PropagateAndRollback("4", List(x2 << (1, 1), x3 << (2, 2), y << (5, 9)), List(x1 << (2, 6)))))
    }

    @Test
    def testSumComputation: Unit = {
        space.post(constraint)
        runScenario(
            TestScenario(
                space,
                Initialize("1", x1 << 2, x2 << 0, x3 << 1, y << 3),
                Initialize("2", x1 << 1, x2 << 2, x3 << 3, y << 6),
                Consult("1", x2 << 3, y << 7),
                Consult("2", x1 << 2, x3 << 2, y << 6),
                ConsultAndCommit("1", x2 << 3, y << 7),
                ConsultAndCommit("2", x1 << 0, x3 << 2, y << 5)))
    }

}
