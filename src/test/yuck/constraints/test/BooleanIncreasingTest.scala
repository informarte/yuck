package yuck.constraints.test

import org.junit.*

import yuck.annealing.DefaultMoveSizeDistribution
import yuck.constraints.test.util.ConstraintTestTooling
import yuck.constraints.{BooleanIncreasing, BooleanIncreasingNeighbourhood}
import yuck.core.*
import yuck.test.util.UnitTest

/**
 * @author Michael Marte
 *
 */
@FixMethodOrder(runners.MethodSorters.NAME_ASCENDING)
final class BooleanIncreasingTest extends UnitTest with ConstraintTestTooling {

    private val randomGenerator = new JavaRandomGenerator
    private val space = new Space(logger, sigint)

    private val xs = for (i <- 1 to 4) yield
        new BooleanVariable(space.nextVariableId(), "x%d".format(i), CompleteBooleanDomain)
    private val Seq(x1, x2, x3, x4) = xs
    private val costs = new BooleanVariable(space.nextVariableId(), "costs", CompleteBooleanDomain)

    @Test
    def testBasics(): Unit = {
        val constraint = new BooleanIncreasing(space.nextConstraintId(), null, xs, costs)
        assertEq(constraint.toString, "increasing([x1, x2, x3, x4], costs)")
        assertEq(constraint.inVariables.size, 4)
        assertEq(constraint.inVariables.toSet, xs.toSet)
        assertEq(constraint.outVariables.size, 1)
        assertEq(constraint.outVariables.head, costs)
    }

    @Test
    def testPropagation(): Unit = {
        space.post(new BooleanIncreasing(space.nextConstraintId(), null, xs, costs))
        runScenario(
            TestScenario(
                space,
                Propagate("do not propagate soft constraint", Nil, Nil),
                PropagateAndRollback("do not propagate negation", List(costs << FalseDomain), Nil),
                Propagate("root-node propagation", List(costs << TrueDomain), Nil),
                PropagateAndRollback("x2 != false", List(x2 << TrueDomain), List(x3 << TrueDomain, x4 << TrueDomain)),
                PropagateAndRollback ("x3 != true", List(x3 << FalseDomain), List(x1 << FalseDomain, x2 << FalseDomain))))
    }

    @Test
    def testHandlingOfConsecutiveDuplicateVariablesInPropagation(): Unit = {
        space.post(new BooleanIncreasing(space.nextConstraintId(), null, Vector(x1, x2, x2, x4), costs))
        runScenario(
            TestScenario(
                space,
                Propagate("root-node propagation", List(costs << TrueDomain), Nil),
                PropagateAndRollback("x1 != false", List(x1 << TrueDomain), List(x2 << TrueDomain, x4 << TrueDomain)),
                PropagateAndRollback("x1 != true", List(x1 << FalseDomain), Nil),
                PropagateAndRollback("x2 != false", List(x2 << TrueDomain), List(x4 << TrueDomain)),
                PropagateAndRollback("x2 != true", List(x2 << FalseDomain), List(x1 << FalseDomain))))
    }

    @Test
    def testHandlingOfNonConsecutiveDuplicateVariablesInPropagation(): Unit = {
        space.post(new BooleanIncreasing(space.nextConstraintId(), null, Vector(x1, x2, x1, x4), costs))
        runScenario(
            TestScenario(
                space,
                Propagate("root-node propagation", List(costs << TrueDomain), Nil),
                PropagateAndRollback("x1 != false", List(x1 << TrueDomain), List(x2 << TrueDomain, x4 << TrueDomain)),
                PropagateAndRollback("x1 != true", List(x1 << FalseDomain), List(x2 << FalseDomain)),
                PropagateAndRollback("x2 != false", List(x2 << TrueDomain), List(x1 << TrueDomain, x4 << TrueDomain)),
                PropagateAndRollback("x2 != true", List(x2 << FalseDomain), List(x1 << FalseDomain))))
    }

    @Test
    def testCostComputation(): Unit = {
        space.post(new BooleanIncreasing(space.nextConstraintId(), null, xs, costs))
        runScenario(
            TestScenario(
                space,
                Initialize("f, f, f, f", x1 << False, x2 << False, x3 << False, x4 << False, costs << True),
                Initialize("t, f, f, f", x1 << True, costs << False),
                Initialize("t, f, t, f", x1 << True, x3 << True, costs << False2),
                Consult("t, t, t, f", x2 << True, costs << False),
                Consult("t, t, t, t", x2 << True, x4 << True, costs << True),
                ConsultAndCommit("t, t, t, t", x2 << True, x4 << True, costs << True),
                ConsultAndCommit("t, f, t, t", x2 << False, costs << False)))
    }

    @Test
    def testHandlingOfConsecutiveDuplicateVariablesInCostComputation(): Unit = {
        space.post(new BooleanIncreasing(space.nextConstraintId(), null, Vector(x1, x2, x2, x4), costs))
        runScenario(
            TestScenario(
                space,
                Initialize("f, f, f, f", x1 << False, x2 << False, x4 << False, costs << True),
                Initialize("t, f, f, f", x1 << True, costs << False),
                Consult("t, t, t, f", x2 << True, costs << False),
                ConsultAndCommit("t, t, t, t", x2 << True, x4 << True, costs << True),
                ConsultAndCommit("t, f, t, t", x2 << False, costs << False)))
    }

    @Test
    def testHandlingOfNonConsecutiveDuplicateVariablesInCostComputation(): Unit = {
        space.post(new BooleanIncreasing(space.nextConstraintId(), null, Vector(x1, x2, x1, x4), costs))
        runScenario(
            TestScenario(
                space,
                Initialize("f, f, f, f", x1 << False, x2 << False, x4 << False, costs << True),
                Initialize("f, t, f, f", x2 << True, costs << False),
                Initialize("t, f, t, f", x1 << True, x2 << False, costs << False2),
                Consult("f, f, f, f", x1 << False, costs << True),
                Consult("t, t, t, t", x2 << True, x4 << True, costs << True),
                Consult("t, t, t, f", x2 << True, costs << False),
                ConsultAndCommit("t, t, t, t", x2 << True, x4 << True, costs << True),
                ConsultAndCommit("t, f, t, t", x2 << False, costs << False)))
    }

    @Test
    def testNeighbourhoodGeneration(): Unit = {
        assertNeighbourhood(xs)
    }

    @Test
    def testHandlingOfConsecutiveDuplicateVariablesInNeighbourhoodGeneration(): Unit = {
        assertNeighbourhood(Vector(x1, x2, x2, x4))
    }

    @Test
    def testHandlingOfNonConsecutiveDuplicateVariablesInNeighbourhoodGeneration(): Unit = {
        assertNoNeighbourhood(Vector(x1, x2, x1, x4))
    }

    @Test
    def testHandlingOfChannelVariablesInNeighbourhoodGeneration(): Unit = {
        import yuck.constraints.Or
        space.post(new Or(space.nextConstraintId(), null, x1, x2, x3))
        assertNoNeighbourhood(xs)
    }

    @Test
    def testHandlingOfFixedAssignmentsInNeighbourhoodGeneration1(): Unit = {
        x1.pruneDomain(FalseDomain)
        assertNeighbourhood(xs)
    }

    @Test
    def testHandlingOfFixedAssignmentsInNeighbourhoodGeneration2(): Unit = {
        x1.pruneDomain(TrueDomain)
        assertNeighbourhood(xs)
    }

    @Test
    def testHandlingOfFixedAssignmentsInNeighbourhoodGeneration3(): Unit = {
        x2.pruneDomain(TrueDomain)
        x3.pruneDomain(FalseDomain)
        assertNoNeighbourhood(xs, isCandidate = true)
    }

    private def assertNeighbourhood(xs: IndexedSeq[BooleanVariable]): Unit = {
        val constraint = new BooleanIncreasing(space.nextConstraintId(), null, xs, costs)
        space.post(constraint)
        assert(constraint.isCandidateForImplicitSolving(space))
        val neighbourhood = constraint.createNeighbourhood(space, randomGenerator, DefaultMoveSizeDistribution).get
        assertEq(neighbourhood.getClass, classOf[BooleanIncreasingNeighbourhood])
        val now = space.searchState
        assert(xs.forall(_.hasValidValue(now)))
        for (i <- 0 until xs.size - 1) {
            assertGe(now.value(xs(i)), now.value(xs(i + 1)))
        }
        assertEq(now.value(costs), True)
        assertEq(neighbourhood.searchVariables, xs.filterNot(_.domain.isSingleton).toSet)
    }

    private def assertNoNeighbourhood(xs: IndexedSeq[BooleanVariable], isCandidate: Boolean = false): Unit = {
        val constraint = new BooleanIncreasing(space.nextConstraintId(), null, xs, costs)
        space.post(constraint)
        assertEq(constraint.isCandidateForImplicitSolving(space), isCandidate)
        assertEq(constraint.createNeighbourhood(space, randomGenerator, DefaultMoveSizeDistribution), None)
    }

}
