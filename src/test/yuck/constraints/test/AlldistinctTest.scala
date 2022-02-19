package yuck.constraints.test

import org.junit.*

import yuck.annealing.DefaultMoveSizeDistribution
import yuck.constraints.test.util.ConstraintTestTooling
import yuck.constraints.{Alldistinct, AlldistinctNeighbourhood}
import yuck.core.*
import yuck.test.util.UnitTest

/**
 * @author Michael Marte
 *
 */
@FixMethodOrder(runners.MethodSorters.NAME_ASCENDING)
final class AlldistinctTest extends UnitTest with ConstraintTestTooling {

    private val randomGenerator = new JavaRandomGenerator
    private val space = new Space(logger, sigint)

    private val baseDomain = IntegerRange(0, 9)
    private val xs = for (i <- 1 to 3) yield new IntegerVariable(space.nextVariableId(), "x%d".format(i), baseDomain)
    private val Seq(x1, x2, x3) = xs
    private val costs = new BooleanVariable(space.nextVariableId(), "costs", CompleteBooleanDomain)

    @Test
    def testBasics(): Unit = {
        val constraint = new Alldistinct(space.nextConstraintId(), null, xs, costs)
        assertEq(constraint.toString, "alldistinct([x1, x2, x3], costs)")
        assertEq(constraint.inVariables.size, 3)
        assertEq(constraint.inVariables.toSet, xs.toSet)
        assertEq(constraint.outVariables.size, 1)
        assertEq(constraint.outVariables.head, costs)
    }

    @Test
    def testPropagation(): Unit = {
        space.post(new Alldistinct(space.nextConstraintId(), null, xs, costs))
        runScenario(
            TestScenario(
                space,
                Propagate("do not propagate soft constraint", Nil, Nil),
                PropagateAndRollback("do not propagate negation", List(costs << FalseDomain), Nil),
                Propagate("root-node propagation", List(costs << TrueDomain), Nil),
                Propagate(
                    "fix x1",
                    List(x1 << ZeroToZeroIntegerRange),
                    List(x2, x3).map(x => x << x.domain.diff(ZeroToZeroIntegerRange)))))
    }

    @Test
    def testHandlingOfDuplicateVariablesInPropagation(): Unit = {
        space.post(new Alldistinct(space.nextConstraintId(), null, Vector(x1, x2, x2), costs))
        runScenario(
            TestScenario(
                space,
                Propagate("root-node propagation", List(costs << TrueDomain), Nil),
                Propagate(
                    "fix value of duplicate variable x2",
                    List(x2 << ZeroToZeroIntegerRange),
                    List(x1 << x1.domain.diff(ZeroToZeroIntegerRange)))))
    }

    @Test
    def testCostComputation(): Unit = {
        space.post(new Alldistinct(space.nextConstraintId(), null, xs, costs))
        runScenario(
            TestScenario(
                space,
                Initialize("no conflict", x1 << 1, x2 << 2, x3 << 3, costs << True),
                Initialize("one conflict", x1 << 1, x2 << 1, x3 << 3, costs << False),
                Initialize("two conflicts", x1 << 1, x2 << 1, x3 << 1, costs << False2),
                Consult("fix a conflict", x3 << 3, costs << False),
                Consult("fix both conflicts", x2 << 2, x3 << 3, costs << True),
                ConsultAndCommit("fix both conflicts", x2 << 2, x3 << 3, costs << True),
                ConsultAndCommit("cause a conflict", x1 << 2, costs << False)))
    }

    @Test
    def testHandlingOfDuplicateVariablesInCostComputation(): Unit = {
        space.post(new Alldistinct(space.nextConstraintId(), null, Vector(x1, x2, x2), costs))
        runScenario(
            TestScenario(
                space,
                Initialize("one conflict", x1 << 1, x2 << 2, costs << False),
                Initialize("two conflicts", x1 << 1, x2 << 1, costs << False2),
                Consult("fix a conflict", x2 << 2, costs << False),
                Consult("move sidewards", x1 << 2, x2 << 2, costs << False2),
                ConsultAndCommit("fix a conflict", x1 << 2, costs << False),
                ConsultAndCommit("cause a conflict", x2 << 2, costs << False2)))
    }

    private def assertNeighbourhood(xs: IndexedSeq[IntegerVariable]): Unit = {
        val constraint = new Alldistinct(space.nextConstraintId(), null, xs, costs)
        space.post(constraint)
        assert(constraint.isCandidateForImplicitSolving(space))
        val neighbourhood =
            constraint.createNeighbourhood(space, randomGenerator, DefaultMoveSizeDistribution, logger, sigint).get
        assertEq(neighbourhood.getClass, classOf[AlldistinctNeighbourhood[IntegerValue]])
        val now = space.searchState
        assert(xs.forall(x => x.domain.contains(now.value(x))))
        assertEq(xs.map(now.value(_)).toSet.size, xs.size)
        assertEq(now.value(costs), True)
        assertEq(neighbourhood.searchVariables, xs.filterNot(_.domain.isSingleton).toSet)
    }

    private def assertNoNeighbourhood(xs: IndexedSeq[IntegerVariable], isCandidate: Boolean = false): Unit = {
        val constraint = new Alldistinct(space.nextConstraintId(), null, xs, costs)
        space.post(constraint)
        assertEq(constraint.isCandidateForImplicitSolving(space), isCandidate)
        assertEq(constraint.createNeighbourhood(space, randomGenerator, DefaultMoveSizeDistribution, logger, sigint), None)
    }

    @Test
    def testNeighbourhoodGenerationWithEqualDomainsAndJustEnoughValues(): Unit = {
        xs.foreach(_.pruneDomain(IntegerRange(0, xs.size - 1)))
        assertNeighbourhood(xs)
    }

    @Test
    def testNeighbourhoodGenerationWithEqualDomainsAndMoreValuesThanVariables(): Unit = {
        assertNeighbourhood(xs)
    }

    @Test
    def testNeighbourhoodGenerationWithDifferentDomainsAndJustEnoughValues(): Unit = {
        xs.indices.foreach(i => xs(i).pruneDomain(IntegerRange(0, xs.size - 1).diff(IntegerDomain(List(i)))))
        assertNeighbourhood(xs)
    }

    @Test
    def testNeighbourhoodGenerationWithDifferentDomainsAndMoreValuesThanVariables(): Unit = {
        xs.indices.foreach(i => xs(i).pruneDomain(xs(i).domain.diff(IntegerDomain(List(i)))))
        assertNeighbourhood(xs)
    }

    @Test
    def testHandlingOfDuplicateVariablesInNeighbourhoodGeneration(): Unit = {
        assertNoNeighbourhood(Vector(x1, x2, x2))
    }

    @Test
    def testHandlingOfChannelVariablesInNeighbourhoodGeneration(): Unit = {
        import yuck.constraints.Plus
        space.post(new Plus(space.nextConstraintId(), null, x1, x2, x3))
        assertNoNeighbourhood(xs)
    }

    @Test
    def testHandlingOfFixedAssignmentsInNeighbourhoodGeneration1(): Unit = {
        x1.pruneDomain(ZeroToZeroIntegerRange)
        space.setValue(x1, Zero)
        assertNoNeighbourhood(xs)
    }

    @Test
    def testHandlingOfFixedAssignmentsInNeighbourhoodGeneration2(): Unit = {
        x2.pruneDomain(x2.domain.diff(ZeroToZeroIntegerRange))
        x3.pruneDomain(x3.domain.diff(ZeroToZeroIntegerRange))
        assertNeighbourhood(xs)
    }

}
