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
final class NumberOfDistinctValuesTest extends UnitTest with ConstraintTestTooling {

    private val space = new Space(logger, sigint)

    private val xs = for (i <- 1 to 3) yield new IntegerVariable(space.nextVariableId(), "x%d".format(i), IntegerRange(1, 2))
    private val Seq(x1, x2, x3) = xs
    private val n = new IntegerVariable(space.nextVariableId(), "n", CompleteIntegerRange)

    @Test
    def testPropagation(): Unit = {
        space.post(new NumberOfDistinctValues(space.nextConstraintId(), null, xs, n))
        runScenario(
            TestScenario(
                space,
                Propagate("root-node propagation", Nil, List(n << IntegerRange(1, 3)))))
    }

    @Test
    def testPropagationWithEmptyXs(): Unit = {
        space.post(new NumberOfDistinctValues[IntegerValue](space.nextConstraintId(), null, Seq(), n))
        runScenario(
            TestScenario(
                space,
                Propagate("root-node propagation", Nil, List(n << IntegerRange(0, 0)))))
    }

    private def testCounting(xs: Seq[IntegerVariable]): Unit = {
        space.post(new NumberOfDistinctValues(space.nextConstraintId(), null, xs, n))
        runScenario(
            TestScenario(
                space,
                Initialize("1", x1 << 1, x2 << 2, x3 << 3, n << 3),
                Initialize("2", x1 << 1, x2 << 1, x3 << 1, n << 1),
                Consult("1", x1 << 2, n << 2),
                Consult("2", x2 << 2, x3 << 2, n << 2),
                ConsultAndCommit("1", x1 << 2, n << 2),
                ConsultAndCommit("2", x2 << 2, x3 << 2, n << 1)))
    }

    @Test
    def testCounting(): Unit = {
        testCounting(xs)
    }

    @Test
    def testHandlingOfDuplicateVariablesInCounting(): Unit = {
        testCounting(List(x1, x2, x2, x3))
    }

}
