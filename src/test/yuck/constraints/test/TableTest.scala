package yuck.constraints.test

import org.junit._

import scala.collection._

import yuck.annealing.DefaultMoveSizeDistribution
import yuck.constraints._
import yuck.core._
import yuck.test.util.UnitTest

/**
 * @author Michael Marte
 *
 */
@FixMethodOrder(runners.MethodSorters.NAME_ASCENDING)
final class TableTest
    extends UnitTest
    with AssignmentPropagationTestTooling
    with DomainPropagationTestTooling
{

    private val randomGenerator = new JavaRandomGenerator
    private val space = new Space(logger, sigint)
    private val now = space.searchState

    private val xs = Vector("s", "t", "u").map(new IntegerVariable(space.nextVariableId, _, IntegerRange(Zero, Nine)))
    private val Vector(s, t, u) = xs
    private val costs = new BooleanVariable(space.nextVariableId, "costs", CompleteBooleanDomain)

    private def createTable(m: Int)(elems: Int*): immutable.IndexedSeq[immutable.IndexedSeq[IntegerValue]] =
        elems.toIndexedSeq.map(IntegerValue.apply).grouped(m).toIndexedSeq

    @Test
    def testConsultAndCommit: Unit = {
        val rows = createTable(3)(0, 0, 0, 1, 2, 3)
        space.post(new Table(space.nextConstraintId, null, xs, rows, costs))
        assertEq(space.searchVariables, xs.toSet)
        runScenario(
            TestScenario(
                space,
                Initialize("initial conflict", (s, One), (t, One), (u, One), (costs, False3)),
                ConsultAndCommit("move away from the first row and approach the second row", (t, Two), (costs, False2)),
                ConsultAndCommit("change two values at once", (s, Zero), (u, Three), (costs, False))))
    }

    @Test
    def testConsultAndCommitWithDuplicateVariables: Unit = {
        val rows = createTable(3)(0, 0, 0, 1, 2, 3, 2, 2, 3)
        space.post(new Table(space.nextConstraintId, null, Vector(s, s, t), rows, costs))
        assertEq(space.searchVariables, Set(s, t))
        runScenario(
            TestScenario(
                space,
                Initialize("initial conflict", (s, One), (t, One), (costs, False3)),
                ConsultAndCommit("fix first two columns", (s, Two), (costs, False2)),
                ConsultAndCommit("fix last column", (t, Three), (costs, True)),
                ConsultAndCommit("change two values at once", (s, Zero), (t, Two), (costs, False2))))
    }

    @Test
    def testPropagation: Unit = {
        s.pruneDomain(IntegerRange(Two, Five))
        t.pruneDomain(IntegerRange(Two, Three))
        costs.pruneDomain(TrueDomain)
        val rows =
            createTable(2)(
                0, 0,
                1, 0, 1, 1, 1, 4,
                2, 0, 2, 2,
                3, 0, 3, 3,
                4, 0, 4, 4,
                5, 0, 5, 1, 5, 2, 5, 3, 5, 4)
        space.post(new Table(space.nextConstraintId, null, Vector(s, t), rows, costs))
        runScenario(
            TestScenario(
                space,
                Propagate(
                    "1",
                    Nil,
                    List((s, IntegerDomain(List(Two, Three, Five))),
                         (t, IntegerDomain(List(Two, Three))))),
                Propagate(
                    "2",
                    (t, IntegerRange(Two, Two)),
                    (s, IntegerDomain(List(Two, Five))))))
    }

    @Test
    def testPropagationWithDuplicateVariables: Unit = {
        s.pruneDomain(IntegerRange(Two, Five))
        costs.pruneDomain(TrueDomain)
        val rows =
            createTable(2)(
                0, 0,
                1, 0, 1, 4,
                2, 0, 2, 2,
                3, 0,
                4, 0, 4, 4,
                5, 0, 5, 1, 5, 2, 5, 3, 5, 4)
        space.post(new Table(space.nextConstraintId, null, Vector(s, s), rows, costs))
        runScenario(
            TestScenario(
                space,
                Propagate(
                    "1",
                    Nil,
                    (s, IntegerDomain(List(Two, Four))))))
    }

    @Test
    def testNeighbourhoodGeneration: Unit = {
        val rows = createTable(3)(0, 0, 0, 1, 2, 3)
        val constraint = new Table(space.nextConstraintId, null, xs, rows, costs)
        space.post(constraint)
        assert(constraint.isCandidateForImplicitSolving(space))
        val neighbourhood =
            constraint.createNeighbourhood(space, randomGenerator, DefaultMoveSizeDistribution, logger, sigint).get
        assertEq(neighbourhood.getClass, classOf[TableNeighbourhood[_]])
        assert(xs.forall(x => x.domain.contains(now.value(x))))
        assert(rows.contains(xs.map(now.value)))
        assertEq(now.value(costs), True)
        assertEq(neighbourhood.searchVariables, xs.toSet)
    }

    @Test
    def testHandlingOfDuplicateVariablesInNeighbourhoodGeneration: Unit = {
        val rows = createTable(3)(0, 0, 0, 1, 2, 3)
        val constraint = new Table(space.nextConstraintId, null, Vector(s, t, s), rows, costs)
        space.post(constraint)
        assert(! constraint.isCandidateForImplicitSolving(space))
        assert(constraint.createNeighbourhood(space, randomGenerator, DefaultMoveSizeDistribution, logger, sigint).isEmpty)
    }

    @Test
    def testHandlingOfChannelVariablesInNeighbourhoodGeneration: Unit = {
        import yuck.constraints.Plus
        val rows = createTable(3)(0, 0, 0, 1, 2, 3)
        val constraint = new Table(space.nextConstraintId, null, xs, rows, costs)
        space.post(constraint)
        space.post(new Plus(space.nextConstraintId, null, s, t, u))
        assert(! constraint.isCandidateForImplicitSolving(space))
        assert(constraint.createNeighbourhood(space, randomGenerator, DefaultMoveSizeDistribution, logger, sigint).isEmpty)
    }

    @Test
    def testHandlingOfFixedColumnsInNeighbourhoodGeneration: Unit = {
        val rows = createTable(3)(0, 0, 0, 1, 2, 3, 1, 3, 4)
        val constraint = new Table(space.nextConstraintId, null, xs, rows, costs)
        space.post(constraint)
        s.pruneDomain(IntegerRange(One, One))
        assert(constraint.isCandidateForImplicitSolving(space))
        val neighbourhood =
            constraint.createNeighbourhood(space, randomGenerator, DefaultMoveSizeDistribution, logger, sigint).get
        assertEq(neighbourhood.getClass, classOf[TableNeighbourhood[_]])
        assert(xs.forall(x => x.domain.contains(now.value(x))))
        assert(rows.contains(xs.map(now.value)))
        assertEq(now.value(costs), True)
        assertEq(neighbourhood.searchVariables, Set(t, u))
    }

}
