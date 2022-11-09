package yuck.constraints.test

import org.junit.*

import yuck.constraints.*
import yuck.constraints.test.util.ConstraintTestTooling
import yuck.core.{given, *}
import yuck.test.util.UnitTest

/**
 * @author Michael Marte
 *
 */
@FixMethodOrder(runners.MethodSorters.NAME_ASCENDING)
final class CountConstTest extends UnitTest with ConstraintTestTooling {

    private val space = new Space(logger, sigint)

    private val baseDomain = IntegerRange(0, 9)
    private val xs = for (i <- 1 to 3) yield new IntegerVariable(space.nextVariableId(), "x%d".format(i), baseDomain)
    private val Seq(x1, x2, x3) = xs
    private val n = new IntegerVariable(space.nextVariableId(), "n", CompleteIntegerRange)

    @Test
    def testBasics(): Unit = {
        val constraint = new CountConst(space.nextConstraintId(), null, xs, One, n)
        assertEq(constraint.toString, "n = count(1, [x1, x2, x3])")
        assertEq(constraint.inVariables.size, xs.size)
        assertEq(constraint.inVariables.toSet, xs.toSet)
        assertEq(constraint.outVariables.size, 1)
        assertEq(constraint.outVariables.head, n)
    }

    @Test
    def testPropagation(): Unit = {
        space.post(new CountConst(space.nextConstraintId(), null, xs, One, n))
        runScenario(
            TestScenario(
                space,
                Propagate("root-node propagation", Nil, List(n << (0, 3))),
                PropagateAndRollback("1", List(x1 << (0, 0)), List(n << (0, 2))),
                PropagateAndRollback("2", List(x2 << (1, 1)), List(n << (1, 3))),
                PropagateAndRollback("3", List(x1 << (0, 0), x2 << (1, 1)), List(n << (1, 2))),
                PropagateAndRollback("4", List(n << (3, 3)), List(x1 << (1, 1), x2 << (1, 1), x3 << (1, 1))),
                Propagate("5", List(x3 << (0, 0), n << (2, 2)), List(x1 << (1, 1), x2 << (1, 1)))))
    }

    @Test
    def testHandlingOfDuplicateVariablesInPropagation(): Unit = {
        space.post(new CountConst(space.nextConstraintId(), null, List(x1, x1, x2, x3), One, n))
        runScenario(
            TestScenario(
                space,
                Propagate("root-node propagation", Nil, List(n << (0, 4))),
                PropagateAndRollback("1", List(x1 << (0, 0)), List(n << (0, 2))),
                PropagateAndRollback("2", List(x2 << (1, 1)), List(n << (1, 4))),
                PropagateAndRollback("3", List(x1 << (0, 0), x2 << (1, 1)), List(n << (1, 2))),
                PropagateAndRollback("4", List(n << (4, 4)), List(x1 << (1, 1), x2 << (1, 1), x3 << (1, 1))),
                Propagate("5", List(x3 << (0, 0), n << (3, 3)), List(x1 << (1, 1), x2 << (1, 1)))))
    }

    @Test
    def testCounting(): Unit = {
        space.post(new CountConst(space.nextConstraintId(), null, xs, One, n))
        runScenario(
            TestScenario(
                space,
                Initialize("1", x1 << 2, x2 << 2, x3 << 2, n << 0),
                Initialize("2", x1 << 1, x2 << 1, x3 << 1, n << 3),
                Consult("1", x1 << 2, n << 2),
                Consult("2", x1 << 2, x3 << 2, n << 1),
                ConsultAndCommit("1", x1 << 2, n << 2),
                ConsultAndCommit("2", x1 << 1, x3 << 2, n << 2),
                ConsultAndCommit("3", x3 << 1, n << 3)))
    }

    @Test
    def testHandlingOfDuplicateVariablesInCounting(): Unit = {
        space.post(new CountConst(space.nextConstraintId(), null, List(x1, x1, x2, x3), One, n))
        runScenario(
            TestScenario(
                space,
                Initialize("1", x1 << 2, x2 << 2, x3 << 2, n << 0),
                Initialize("2", x1 << 1, x2 << 1, x3 << 1, n << 4),
                Consult("1", x1 << 2, n << 2),
                Consult("2", x1 << 2, x3 << 2, n << 1),
                ConsultAndCommit("1", x1 << 2, n << 2),
                ConsultAndCommit("2", x1 << 1, x3 << 2, n << 3),
                ConsultAndCommit("3", x3 << 1, n << 4)))
    }

    @Test
    def testNormalizationOfBooleanValuesInCounting(): Unit = {
        val xs = for (i <- 1 to 3) yield new BooleanVariable(space.nextVariableId(), "x%d".format(i), CompleteBooleanDomain)
        val Seq(x1, x2, x3) = xs
        space.post(new CountConst(space.nextConstraintId(), null, xs, False, n))
        runScenario(
            TestScenario(
                space,
                Initialize("1", x1 << False, x2 << False2, x3 << False3, n << 3),
                Initialize("2", x1 << True, x2 << True, x3 << True, n << 0),
                Consult("1", x1 << False, n << 1),
                Consult("2", x1 << False2, n << 1),
                ConsultAndCommit("1", x1 << False2, n << 1),
                ConsultAndCommit("2", x2 << False3, x3 << False, n << 3)))
    }

}
