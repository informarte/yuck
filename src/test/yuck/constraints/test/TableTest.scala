package yuck.constraints.test

import org.junit.*

import yuck.annealing.DefaultMoveSizeDistribution
import yuck.constraints.test.util.ConstraintTestTooling
import yuck.constraints.{Table, TableNeighbourhood}
import yuck.core.{given, *}
import yuck.test.util.UnitTest

/**
 * @author Michael Marte
 *
 */
@FixMethodOrder(runners.MethodSorters.NAME_ASCENDING)
final class TableTest extends UnitTest with ConstraintTestTooling {

    private val randomGenerator = new JavaRandomGenerator
    private val space = new Space(logger, sigint)

    private val xs = for (i <- 1 to 3) yield new IntegerVariable(space.nextVariableId(), "x%d".format(i), IntegerRange(0, 9))
    private val Seq(x1, x2, x3) = xs
    private val costs = new BooleanVariable(space.nextVariableId(), "costs", CompleteBooleanDomain)

    private def createTable(m: Int)(elems: Int*): IndexedSeq[IndexedSeq[IntegerValue]] =
        elems.toVector.map(IntegerValue.apply).grouped(m).toVector

    @Test
    def testBasics(): Unit = {
        val rows = createTable(3)(0, 0, 0, 1, 2, 3)
        val constraint = new Table(space.nextConstraintId(), null, xs, rows, costs)
        assertEq(constraint.toString, "table([x1, x2, x3], [[0, 0, 0], [1, 2, 3]], costs)")
        assertEq(constraint.inVariables.size, 3)
        assertEq(constraint.inVariables.toSet, xs.toSet)
        assertEq(constraint.outVariables.size, 1)
        assertEq(constraint.outVariables.head, costs)
    }

    @Test
    def testPropagation(): Unit = {
        val rows =
            createTable(2)(
                0, 0,
                1, 0, 1, 1, 1, 4,
                2, 0, 2, 2,
                3, 0, 3, 3,
                4, 0, 4, 4,
                5, 0, 5, 1, 5, 2, 5, 3, 5, 4)
        space.post(new Table(space.nextConstraintId(), null, Vector(x1, x2), rows, costs))
        runScenario(
            TestScenario(
                space,
                Propagate("do not propagate soft constraint", Nil, Nil),
                PropagateAndRollback("do not propagate negation", List(costs << FalseDomain), Nil),
                Propagate("root-node propagation", List(costs << TrueDomain), List(x1 << (0, 5), x2 << (0, 4))),
                Propagate("limit options", List(x1 << (2, 5), x2 << (2, 3)), List(x1 << List(2, 3, 5), x2 << (2, 3))),
                Propagate("fix first column", List(x1 << (2, 2)), List(x2 << (2, 2)))))
    }

    @Test
    def testHandlingOfDuplicateVariablesInPropagation(): Unit = {
        val rows =
            createTable(2)(
                0, 0,
                1, 0, 1, 4,
                2, 0, 2, 2,
                3, 0,
                4, 0, 4, 4,
                5, 0, 5, 1, 5, 2, 5, 3, 5, 4)
        space.post(new Table(space.nextConstraintId(), null, Vector(x1, x1), rows, costs))
        runScenario(
            TestScenario(
                space,
                Propagate("root-node propagation", List(costs << TrueDomain), List(x1 << List(0, 2, 4))),
                Propagate("limit options", List(x1 << (2, 5), x2 << (2, 3)), List(x1 << List(2, 4))),
                Propagate("fix columns", List(x1 << (2, 2)), Nil)))
    }

    @Test
    def testCostComputation(): Unit = {
        val rows = createTable(3)(0, 0, 0, 1, 2, 3)
        space.post(new Table(space.nextConstraintId(), null, xs, rows, costs))
        runScenario(
            TestScenario(
                space,
                Initialize("select tuple closer to first row", x1 << 1, x2 << 1, x3 << 0, costs << False2),
                Initialize("select first row", x1 << 0, x2 << 0, x3 << 0, costs << True),
                Initialize("select tuple closer to second row", x1 << 1, x2 << 2, x3 << 1, costs << False2),
                Initialize("select second row", x1 << 1, x2 << 2, x3 << 3, costs << True),
                Initialize("select tuple between first and second rows", x1 << 1, x2 << 1, x3 << 1, costs << False3),
                Consult("approach first row", x1 << 0, x2 << 0, costs << False),
                Consult("move to first row", x1 << 0, x2 << 0, x3 << 0, costs << True),
                Consult("approach second row", x2 << 2, costs << False2),
                Consult("move to second row", x2 << 2, x3 << 3, costs << True),
                ConsultAndCommit("approach first row", x1 << 0, x2 << 0, costs << False),
                ConsultAndCommit("select first row", x3 << 0, costs << True),
                ConsultAndCommit("approach second row", x1 << 1, x2 << 2, x3 << 1, costs << False2),
                ConsultAndCommit("select second row", x3 << 3, costs << True)))
    }

    @Test
    def testHandlingOfDuplicateVariablesInCostComputation(): Unit = {
        val rows = createTable(3)(0, 0, 0, 1, 2, 3, 2, 2, 3)
        space.post(new Table(space.nextConstraintId(), null, Vector(x1, x1, x2), rows, costs))
        runScenario(
            TestScenario(
                space,
                Initialize("select first row", x1 << 0, x2 << 0, costs << True),
                Initialize("select third row", x1 << 2, x2 << 3, costs << True),
                Initialize("select tuple closer to first and second rows", x1 << 1, x2 << 1, costs << False3),
                Consult("approach first row", x1 << 0, costs << False),
                Consult("select first row", x1 << 0, x2 << 0, costs << True),
                Consult("approach third row", x1 << 2, costs << False2),
                Consult("select third row", x1 << 2, x2 << 3, costs << True),
                ConsultAndCommit("approach first row", x1 << 0, costs << False),
                ConsultAndCommit("select first row", x2 << 0, costs << True),
                ConsultAndCommit("approach third row", x1 << 2, x2 << 1, costs << False2),
                ConsultAndCommit("select third row", x2 << 3, costs << True)))
    }

    @Test
    def testNeighbourhoodGeneration(): Unit = {
        assertNeighbourhood(xs, createTable(3)(0, 0, 0, 1, 2, 3))
    }

    @Test
    def testHandlingOfDuplicateVariablesInNeighbourhoodGeneration(): Unit = {
        assertNoNeighbourhood(Vector(x1, x2, x1), createTable(3)(0, 0, 0, 1, 2, 3))
    }

    @Test
    def testHandlingOfChannelVariablesInNeighbourhoodGeneration(): Unit = {
        import yuck.constraints.Plus
        space.post(new Plus(space.nextConstraintId(), null, x1, x2, x3))
        assertNoNeighbourhood(xs, createTable(3)(0, 0, 0, 1, 2, 3))
    }

    @Test
    def testHandlingOfFixedColumnsInNeighbourhoodGeneration(): Unit = {
        x1.pruneDomain(IntegerDomain(1))
        assertNeighbourhood(xs, createTable(3)(0, 0, 0, 1, 2, 3, 1, 3, 4))
    }

    private def assertNeighbourhood
        (xs: IndexedSeq[IntegerVariable],
         rows: IndexedSeq[IndexedSeq[IntegerValue]]):
        Unit =
    {
        val constraint = new Table(space.nextConstraintId(), null, xs, rows, costs)
        space.post(constraint)
        assert(constraint.isCandidateForImplicitSolving(space))
        val neighbourhood = constraint.createNeighbourhood(space, randomGenerator, DefaultMoveSizeDistribution).get
        assertEq(neighbourhood.getClass, classOf[TableNeighbourhood[IntegerValue]])
        val now = space.searchState
        assert(xs.forall(_.hasValidValue(now)))
        assert(rows.contains(xs.map(now.value(_))))
        assertEq(now.value(costs), True)
        assertEq(neighbourhood.searchVariables, xs.filterNot(_.domain.isSingleton).toSet)
    }

    private def assertNoNeighbourhood
        (xs: IndexedSeq[IntegerVariable],
         rows: IndexedSeq[IndexedSeq[IntegerValue]],
         isCandidate: Boolean = false):
        Unit =
    {
        val constraint = new Table(space.nextConstraintId(), null, xs, rows, costs)
        space.post(constraint)
        assertEq(constraint.isCandidateForImplicitSolving(space), isCandidate)
        assertEq(constraint.createNeighbourhood(space, randomGenerator, DefaultMoveSizeDistribution), None)
    }

}
