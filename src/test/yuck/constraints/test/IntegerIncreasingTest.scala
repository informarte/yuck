package yuck.constraints.test

import org.junit.*

import scala.jdk.CollectionConverters.*

import yuck.annealing.DefaultMoveSizeDistribution
import yuck.constraints.test.util.ConstraintTestTooling
import yuck.constraints.{IntegerIncreasing, IntegerIncreasingNeighbourhood}
import yuck.core.{*, given}
import yuck.test.*
import yuck.test.util.UnitTest

/**
 * @author Michael Marte
 *
 */
@FixMethodOrder(runners.MethodSorters.NAME_ASCENDING)
@runner.RunWith(classOf[runners.Parameterized])
final class IntegerIncreasingTest(strict: Boolean) extends UnitTest with ConstraintTestTooling {

    private val randomGenerator = new JavaRandomGenerator
    private val space = new Space(logger, sigint)

    private val xs = for (i <- 1 to 4) yield new IntegerVariable(space.nextVariableId(), "x%d".format(i), IntegerRange(1, 4))
    private val Seq(x1, x2, x3, x4) = xs
    private val costs = new BooleanVariable(space.nextVariableId(), "costs", CompleteBooleanDomain)

    @Test
    def testBasics(): Unit = {
        val constraint = new IntegerIncreasing(space.nextConstraintId(), null, xs, strict, costs)
        assertEq(constraint.toString, "increasing([x1, x2, x3, x4], %s, costs)".format(strict))
        assertEq(constraint.inVariables.size, 4)
        assertEq(constraint.inVariables.toSet, xs.toSet)
        assertEq(constraint.outVariables.size, 1)
        assertEq(constraint.outVariables.head, costs)
    }

    @Test
    def testPropagation(): Unit = {
        space.post(new IntegerIncreasing(space.nextConstraintId(), null, xs, strict, costs))
        if (strict) {
            runScenario(
                TestScenario(
                    space,
                    Propagate("do not propagate soft constraint", Nil, Nil),
                    PropagateAndRollback("do not propagate negation", List(costs << FalseDomain), Nil),
                    Propagate(
                        "root-node propagation",
                        List(costs << TrueDomain),
                        List(x1 << (1, 1), x2 << (2, 2), x3 << (3, 3), x4 << (4, 4)))))
        } else {
            runScenario(
                TestScenario(
                    space,
                    Propagate("do not propagate soft constraint", Nil, Nil),
                    PropagateAndRollback("do not propagate negation", List(costs << FalseDomain), Nil),
                    Propagate("root-node propagation", List(costs << TrueDomain), Nil),
                    PropagateAndRollback("x2 >= 3", List(x2 << (3, 4)), List(x3 << (3, 4), x4 << (3, 4))),
                    PropagateAndRollback("x3 < 3", List(x3 << (1, 2)), List(x1 << (1, 2), x2 << (1, 2)))))
        }
    }

    @Test
    def testHandlingOfConsecutiveDuplicateVariablesInPropagation(): Unit = {
        space.post(new IntegerIncreasing(space.nextConstraintId(), null, Vector(x1, x2, x2, x4), strict, costs))
        if (strict) {
            runScenario(
                TestScenario(
                    space,
                    Propagate(
                        "root-node propagation",
                        List(costs << TrueDomain),
                        List(x1 << (1, 3), x2 << EmptyIntegerRange, x4 << (4, 4)))))
        } else {
            runScenario(
                TestScenario(
                    space,
                    Propagate("root-node propagation", List(costs << TrueDomain), Nil),
                    PropagateAndRollback("x2 >= 3", List(x2 << (3, 4)), List(x4 << (3, 4))),
                    PropagateAndRollback("x3 < 3", List(x4 << (1, 2)), List(x1 << (1, 2), x2 << (1, 2)))))
        }
    }

    @Test
    def testHandlingOfNonConsecutiveDuplicateVariablesInPropagation(): Unit = {
        space.post(new IntegerIncreasing(space.nextConstraintId(), null, Vector(x1, x2, x1, x4), strict, costs))
        if (strict) {
            runScenario(
                TestScenario(
                    space,
                    Propagate(
                        "root-node propagation",
                        List(costs << TrueDomain),
                        List(x1 << EmptyIntegerRange, x2 << (2, 2), x4 << (4, 4)))))
        } else {
            runScenario(
                TestScenario(
                    space,
                    Propagate("root-node propagation", List(costs << TrueDomain), Nil),
                    PropagateAndRollback("x2 >= 3", List(x1 << (3, 4)), List(x2 << (3, 4), x4 << (3, 4))),
                    PropagateAndRollback("x3 < 3", List(x4 << (1, 2)), List(x1 << (1, 2), x2 << (1, 2)))))
        }
    }

    @Test
    def testCostComputation(): Unit = {
        space.post(new IntegerIncreasing(space.nextConstraintId(), null, xs, strict, costs))
        if (strict) {
            runScenario(
                TestScenario(
                    space,
                    Initialize("1, 2, 3, 4", x1 << 1, x2 << 2, x3 << 3, x4 << 4, costs << True),
                    Initialize("2, 2, 3, 4", x1 << 2, costs << False),
                    Initialize("2, 2, 4, 4", x3 << 4, costs << False2),
                    Consult("1, 2, 4, 4", x1 << 1, costs << False),
                    Consult("1, 2, 3, 4", x1 << 1, x3 << 3, costs << True),
                    ConsultAndCommit("1, 2, 3, 4", x1 << 1, x3 << 3, costs << True),
                    ConsultAndCommit("1, 2, 3, 1", x4 << 1, costs << False3)))
        } else {
            runScenario(
                TestScenario(
                    space,
                    Initialize("1, 1, 1, 1", x1 << 1, x2 << 1, x3 << 1, x4 << 1, costs << True),
                    Initialize("3, 1, 1, 1", x1 << 3, costs << False2),
                    Initialize("2, 1, 2, 1", x1 << 2, x3 << 2, costs << False2),
                    Consult("2, 2, 2, 1", x2 << 2, costs << False),
                    Consult("2, 2, 2, 2", x2 << 2, x4 << 2, costs << True),
                    ConsultAndCommit("2, 2, 2, 2", x2 << 2, x4 << 2, costs << True),
                    ConsultAndCommit("2, 4, 2, 2", x2 << 4, costs << False2)))
        }
    }

    @Test
    def testHandlingOfConsecutiveDuplicateVariablesInCostComputation(): Unit = {
        space.post(new IntegerIncreasing(space.nextConstraintId(), null, Vector(x1, x2, x2, x4), strict, costs))
        if (strict) {
            runScenario(
                TestScenario(
                    space,
                    Initialize("1, 2, 2, 4", x1 << 1, x2 << 2, x4 << 4, costs << False),
                    Initialize("2, 2, 2, 4", x1 << 2, costs << False2),
                    Consult("2, 1, 2, 4", x2 << 1, costs << False3),
                    ConsultAndCommit("1, 2, 2, 2", x1 << 1, x4 << 2, costs << False2),
                    ConsultAndCommit("1, 2, 2, 1", x4 << 1, costs << False3)))
        } else {
            runScenario(
                TestScenario(
                    space,
                    Initialize("1, 1, 1, 1", x1 << 1, x2 << 1, x4 << 1, costs << True),
                    Initialize("2, 1, 1, 1", x1 << 2, costs << False),
                    Consult("2, 2, 2, 1", x2 << 2, costs << False),
                    ConsultAndCommit("2, 2, 2, 2", x2 << 2, x4 << 2, costs << True),
                    ConsultAndCommit("2, 2, 2, 1", x4 << 1, costs << False)))
        }
    }

    @Test
    def testHandlingOfNonConsecutiveDuplicateVariablesInCostComputation(): Unit = {
        space.post(new IntegerIncreasing(space.nextConstraintId(), null, Vector(x1, x2, x1, x4), strict, costs))
        if (strict) {
            runScenario(
                TestScenario(
                    space,
                    Initialize("1, 2, 1, 4", x1 << 1, x2 << 2, x4 << 4, costs << False2),
                    Consult("1, 3, 1, 4", x2 << 3, costs << False3),
                    ConsultAndCommit("1, 2, 1, 4", x1 << 2, costs << False2),
                    ConsultAndCommit("4, 2, 4, 4", x1 << 4, costs << False4)))
        } else {
            runScenario(
                TestScenario(
                    space,
                    Initialize("1, 1, 1, 1", x1 << 1, x2 << 1, x4 << 1, costs << True),
                    Initialize("1, 2, 1, 1", x2 << 2, costs << False),
                    Initialize("2, 1, 2, 1", x1 << 2, x2 << 1, costs << False2),
                    Consult("1, 1, 1, 1", x1 << 1, costs << True),
                    Consult("2, 2, 2, 2", x2 << 2, x4 << 2, costs << True),
                    Consult("2, 1, 2, 2", x4 << 2, costs << False),
                    ConsultAndCommit("2, 2, 2, 2", x2 << 2, x4 << 2, costs << True),
                    ConsultAndCommit("2, 2, 2, 1", x4 << 1, costs << False)))
        }
    }

    @Test
    def testNeighbourhoodGeneration(): Unit = {
        assertNeighbourhood(xs)
    }

    @Test
    def testHandlingOfConsecutiveDuplicateVariablesInNeighbourhoodGeneration(): Unit = {
        val xs = Vector(x1, x2, x2, x4)
        if (strict) {
            assertNoNeighbourhood(xs)
        } else {
            assertNeighbourhood(xs)
        }
    }

    @Test
    def testHandlingOfNonConsecutiveDuplicateVariablesInNeighbourhoodGeneration(): Unit = {
        assertNoNeighbourhood(Vector(x1, x2, x1, x4))
    }

    @Test
    def testHandlingOfChannelVariablesInNeighbourhoodGeneration(): Unit = {
        import yuck.constraints.Plus
        space.post(new Plus(space.nextConstraintId(), null, x1, x2, x3))
        assertNoNeighbourhood(xs)
    }

    @Test
    def testHandlingOfFixedAssignmentsInNeighbourhoodGeneration1(): Unit = {
        x1.pruneDomain(IntegerRange(1, 1))
        assertNeighbourhood(xs)
    }

    @Test
    def testHandlingOfFixedAssignmentsInNeighbourhoodGeneration2(): Unit = {
        x1.pruneDomain(IntegerRange(4, 4))
        if (strict) {
            assertNoNeighbourhood(xs, isCandidate = true)
        } else {
            assertNeighbourhood(xs)
        }
    }

    @Test
    def testHandlingOfFixedAssignmentsInNeighbourhoodGeneration3(): Unit = {
        x2.pruneDomain(IntegerRange(3, 4))
        x3.pruneDomain(if strict then IntegerRange(2, 3) else IntegerRange(1, 2))
        assertNoNeighbourhood(xs, isCandidate = true)
    }

    private def assertNeighbourhood(xs: IndexedSeq[IntegerVariable]): Unit = {
        val constraint = new IntegerIncreasing(space.nextConstraintId(), null, xs, strict, costs)
        space.post(constraint)
        assert(constraint.isCandidateForImplicitSolving(space))
        val neighbourhood = constraint.createNeighbourhood(space, randomGenerator, DefaultMoveSizeDistribution).get
        assertEq(neighbourhood.getClass, classOf[IntegerIncreasingNeighbourhood])
        val now = space.searchState
        assert(xs.forall(_.hasValidValue(now)))
        for (i <- 0 until xs.size - 1) {
            val a = now.value(xs(i))
            val b = now.value(xs(i + 1))
            if (strict) {
                assertLt(a, b)
            } else {
                assertLe(a, b)
            }
        }
        assertEq(now.value(costs), True)
        assertEq(neighbourhood.searchVariables, xs.filterNot(_.domain.isSingleton).toSet)
    }

    private def assertNoNeighbourhood(xs: IndexedSeq[IntegerVariable], isCandidate: Boolean = false): Unit = {
        val constraint = new IntegerIncreasing(space.nextConstraintId(), null, xs, strict, costs)
        space.post(constraint)
        assertEq(constraint.isCandidateForImplicitSolving(space), isCandidate)
        assertEq(constraint.createNeighbourhood(space, randomGenerator, DefaultMoveSizeDistribution), None)
    }

}

/**
 * @author Michael Marte
 *
 */
object IntegerIncreasingTest {

    @runners.Parameterized.Parameters(name = "{index}: {0}")
    def parameters = List(false, true).asJava

}
