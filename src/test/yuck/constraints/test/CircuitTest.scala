package yuck.constraints.test

import org.junit.*

import scala.jdk.CollectionConverters.*

import yuck.annealing.DefaultMoveSizeDistribution
import yuck.constraints.test.util.ConstraintTestTooling
import yuck.constraints.{Circuit, CircuitNeighbourhood, CircuitTracker}
import yuck.core.*
import yuck.test.util.UnitTest

/**
 * @author Michael Marte
 *
 */
@FixMethodOrder(runners.MethodSorters.NAME_ASCENDING)
@runner.RunWith(classOf[runners.Parameterized])
final class CircuitTest(offset: Int) extends UnitTest with ConstraintTestTooling {

    private val invalidIndex1 = IntegerValue(offset - 1)
    private val one = IntegerValue(offset)
    private val two = IntegerValue(offset + 1)
    private val three = IntegerValue(offset + 2)
    private val four = IntegerValue(offset + 3)
    private val five = IntegerValue(offset + 4)
    private val invalidIndex2 = IntegerValue(offset + 5)
    private val validIndexRange = IntegerRange(one, five)

    private val randomGenerator = new JavaRandomGenerator
    private val space = new Space(logger, sigint)
    private val now = space.searchState

    private val baseDomain = IntegerRange(invalidIndex1, invalidIndex2)
    private val succ = for (i <- 1 to 5) yield new IntegerVariable(space.nextVariableId(), "x%d".format(i), baseDomain)
    private val Seq(x1, x2, x3, x4, x5) = succ
    private val costs = new BooleanVariable(space.nextVariableId(), "costs", CompleteBooleanDomain)

    @Test
    def testBasics(): Unit = {
        val constraint = new Circuit(space.nextConstraintId(), null, succ, offset, costs, logger, sigint)
        assertEq(constraint.toString, "circuit([%s], %d, %s)".format(succ.mkString(", "), offset, costs))
        assertEq(constraint.inVariables.size, succ.size)
        assertEq(constraint.inVariables.toSet, succ.toSet)
        assertEq(constraint.outVariables.size, 1)
        assertEq(constraint.outVariables.head, costs)
    }

    @Test
    def testPropagation(): Unit = {
        space.post(new Circuit(space.nextConstraintId(), null, succ, offset, costs, logger, sigint))
        runScenario(
            TestScenario(
                space,
                Propagate("do not propagate soft constraint", Nil, Nil),
                PropagateAndRollback("do not propagate negation", List(costs << FalseDomain), Nil),
                Propagate(
                    "root-node propagation",
                    List(costs << TrueDomain),
                    succ.indices.map(i => succ(i) << validIndexRange.diff(IntegerDomain(offset + i)))),
                Propagate(
                    "x1 -> x3",
                    List(x1 << IntegerDomain(three)),
                    succ.indices.map(i =>
                        succ(i) <<
                            (if (i == 0) IntegerDomain(three)
                             else validIndexRange.diff(IntegerDomain(IntegerValue(offset + i), three)))))))
    }

    @Test
    def testHandlingOfDuplicateVariablesInPropagation(): Unit = {
        space.post(new Circuit(space.nextConstraintId(), null, Vector(x1, x2, x3, x4, x1), offset, costs, logger, sigint))
        runScenario(
            TestScenario(
                space,
                Propagate(
                    "root-node propagation",
                    List(costs << TrueDomain),
                    succ.indices.diff(List(4)).map(i =>
                        if (i == 0) succ(0) << validIndexRange.diff(IntegerDomain(offset, offset + 4))
                        else succ(i) << validIndexRange.diff(IntegerDomain(offset + i))))))
    }

    @Test
    def testCostComputation(): Unit = {
        space.post(new Circuit(space.nextConstraintId(), null, succ, offset, costs, logger, sigint))
        runScenario(
            TestScenario(
                space,
                Initialize("1 -> 2 -> 3 -> 4 -> 5 -> 1 (circuit)", x1 << two, x2 << three, x3 << four, x4 << five, x5 << one, costs << True),
                Initialize("1 -> 3 -> 5 -> 2 -> 4 -> 1 (circuit)", x1 << three, x2 << four, x3 << five, x4 << one, x5 << two, costs << True),
                Initialize("1 -> 2 -> 3 -> 4 -> 3, 5 -> 4 (1 subcircuit, 2 converging arcs)", x1 << two, x2 << three, x3 << four, x4 << three, x5 << four, costs << False3),
                Initialize("1 -> 2 -> 1, 3 -> 4 -> 5 -> 3 (2 subcircuits)", x1 << two, x2 << one, x3 << four, x4 << five, x5 << three, costs << False2),
                Initialize("1 -> 1, 2 -> 1, 3 -> 1, 4 -> 1, 5 -> 1 (1 subcircuit, 5 converging arcs)", x1 << one, x2 << one, x3 << one, x4 << one, x5 << one, costs << False4),
                Initialize("1 -> 1, 2 -> 2, 3 -> 3, 4 -> 4, 5 -> 5 (5 subcircuits)", x1 << one, x2 << two, x3 << three, x4 << four, x5 << five, costs << False4),
                Consult("1 -> 2 -> 2, 3 -> 3, 4 -> 4, 5 -> 5 (4 subcircuits, 2 converging arcs)", x1 << two, costs << False4),
                Consult("1 -> 1, 2 -> 2, 3 -> 4 -> 3, 5 -> 5 (4 subcircuits)", x3 << four, x4 << three, costs << False3),
                Consult("1 -> 3 -> 5 -> 2 -> 4 -> 1 (circuit)", x1 << three, x2 << four, x3 << five, x4 << one, x5 << two, costs << True),
                ConsultAndCommit("1 -> 1, 2 -> 3 -> 3, 4 -> 4, 5 -> 5 (3 subcircuits, 2 converging arcs)", x2 << three, costs << False4),
                ConsultAndCommit("1 -> 1, 2 -> 3 -> 2, 4 -> 4, 5 -> 5 (3 subcircuits)", x3 << two, costs << False3),
                ConsultAndCommit("1 -> 2 -> 3 -> 2, 4 -> 4, 5 -> 5 (3 subcircuits, 2 converging arcs)", x1 << two, costs << False3),
                ConsultAndCommit("1 -> 2 -> 3 -> 2, 4 -> 5 -> 5 (2 subcircuits, 2 + 2 converging arcs)", x4 << five, costs << False3),
                ConsultAndCommit("1 -> 2 -> 3 -> 2, 4 -> 5 -> 1 (1 subcircuit, 2 converging arcs)", x5 << one, costs << False3),
                ConsultAndCommit("1 -> 2 -> 3 -> 4 -> 5 -> 1 (circuit)", x3 << four, costs << True),
                ConsultAndCommit("1 -> 2 -> 3 -> 4 -> 5 -> 2 (1 subcircuit, 2 converging arcs)", x5 << two, costs << False),
                ConsultAndCommit("1 -> 2 -> 4 -> 5 -> 2, 3 -> 4 (1 subcircuit, 2 converging arcs)", x2 << four, costs << False2),
                ConsultAndCommit("1 -> 2 -> 3 -> 4 -> 5 -> 1 (circuit)", x2 << three, x5 << one, costs << True),
                ConsultAndCommit("1 -> 3 -> 5 -> 2 -> 4 -> 1 (circuit)", x1 << three, x2 << four, x3 << five, x4 << one, x5 << two, costs << True)))
    }

    @Test
    def testHandlingOfDuplicateVariablesInCostComputation(): Unit = {
        space.post(new Circuit(space.nextConstraintId(), null, Vector(x1, x2, x3, x4, x1), offset, costs, logger, sigint))
        runScenario(
            TestScenario(
                space,
                Initialize("1 -> 2 -> 3 -> 4 -> 5 -> 2", x1 << two, x2 << three, x3 << four, x4 << five, costs << False),
                Initialize("5 -> 1 -> 1, 2 -> 3 -> 4 -> 4", x1 << one, x2 << three, x3 << four, x4 << four, costs << False4),
                Consult("1 -> 3, 2 -> 3 -> 4 -> 4, 5 -> 3", x1 << three, costs << False4),
                Consult("5 -> 1 -> 1, 2 -> 3 -> 4 -> 2", x4 << two, costs << False2),
                ConsultAndCommit("1 -> 3 -> 4 -> 4, 2 -> 3, 5 -> 2", x1 << three, costs << False4),
                ConsultAndCommit("1 -> 3 -> 4 -> 2 -> 3, 5 -> 2", x4 << two, costs << False2)))
    }

    @Test
    def testHandlingOfInvalidNodeReferencesInCostComputation(): Unit = {
        val constraint = new Circuit(space.nextConstraintId(), null, succ, offset, costs, logger, sigint)
        space.post(constraint)
        runScenario(
            TestScenario(
                space,
                Initialize("1 -> ?, 2 -> ?, 3 -> 4 -> 5 -> 1", x1 << invalidIndex1, x2 << invalidIndex2, x3 << four, x4 << five, x5 << three, costs << False2),
                Initialize("1 -> 2 -> 1, 3 -> 4 -> 5 -> 3", x1 << two, x2 << one, x3 << four, x4 << five, x5 << three, costs << False2),
                Consult("1 -> 2 -> ?, 3 -> 4 -> 5 -> 3", x2 << invalidIndex1, costs << False2),
                Consult("1 -> 2 -> 1, 3 -> 4 -> 5 -> ?", x5 << invalidIndex2, costs << False3),
                ConsultAndCommit("1 -> 2 -> ?, 3 -> 4 -> 5 -> 3", x2 << invalidIndex1, costs << False2),
                ConsultAndCommit("1 -> 2 -> ?, 3 -> 4 -> 5 -> ?", x5 << invalidIndex2, costs << False5)))
    }

    @Test
    def testHamiltonianCircuitTest(): Unit = {
        for (i <- succ.indices) {
            space.setValue(succ(i), IntegerValue(offset + i + 1))
        }
        assert(! Circuit.isHamiltonianCircuit(succ, offset, now))
        space.setValue(succ.last, IntegerValue(offset))
        assert(Circuit.isHamiltonianCircuit(succ, offset, now))
        space.setValue(succ(0), IntegerValue(offset))
        assert(! Circuit.isHamiltonianCircuit(succ, offset, now))
        space.setValue(succ(0), IntegerValue(offset + 2))
        assert(! Circuit.isHamiltonianCircuit(succ, offset, now))
    }

    @Test
    def testNeighbourhoodGeneration(): Unit = {
        assertNeighbourhood(succ)
    }

    @Test
    def testHandlingOfDuplicateVariablesInNeighbourhoodGeneration(): Unit = {
        assertNoNeighbourhood(Vector(x1, x2, x3, x4, x1))
    }

    @Test
    def testHandlingOfChannelVariablesInNeighbourhoodGeneration(): Unit = {
        import yuck.constraints.Plus
        space.post(new Plus(space.nextConstraintId(), null, x1, x2, x3))
        assertNoNeighbourhood(succ)
    }

    @Test
    def testHandlingOfFixedArcsInNeighbourhoodGeneration(): Unit = {
        x1.pruneDomain(IntegerRange(three, three))
        assertNeighbourhood(succ)
    }

    @Test
    def testHandlingOfConvergingArcsInNeighbourhoodGeneration(): Unit = {
        x1.pruneDomain(IntegerRange(three, three))
        x2.pruneDomain(IntegerRange(three, three))
        assertNoNeighbourhood(succ, true)
    }

    @Test
    def testHandlingOfSubcircuitsInNeighbourhoodGeneration(): Unit = {
        x1.pruneDomain(IntegerRange(two, two))
        x2.pruneDomain(IntegerRange(three, three))
        x3.pruneDomain(IntegerRange(one, one))
        assertNoNeighbourhood(succ, true)
    }

    private def assertNeighbourhood(succ: IndexedSeq[IntegerVariable]): Unit = {
        val constraint = new Circuit(space.nextConstraintId(), null, succ, offset, costs, logger, sigint)
        space.post(constraint)
        assert(constraint.isCandidateForImplicitSolving(space))
        val neighbourhood = constraint.createNeighbourhood(space, randomGenerator, DefaultMoveSizeDistribution).get
        assertEq(neighbourhood.getClass, classOf[CircuitNeighbourhood])
        assert(succ.forall(x => x.domain.contains(now.value(x))))
        assert(Circuit.isHamiltonianCircuit(succ, offset, now))
        assertEq(now.value(costs), True)
        assertEq(neighbourhood.searchVariables, succ.filterNot(_.domain.isSingleton).toSet)
    }

    private def assertNoNeighbourhood(succ: IndexedSeq[IntegerVariable], isCandidate: Boolean = false): Unit = {
        val constraint = new Circuit(space.nextConstraintId(), null, succ, offset, costs, logger, sigint)
        space.post(constraint)
        assertEq(constraint.isCandidateForImplicitSolving(space), isCandidate)
        assertEq(constraint.createNeighbourhood(space, randomGenerator, DefaultMoveSizeDistribution), None)
    }

}

/**
 * @author Michael Marte
 *
 */
object CircuitTest {

    @runners.Parameterized.Parameters(name = "{index}: {0}")
    def parameters = List(-1, 0, 1).asJava

}
