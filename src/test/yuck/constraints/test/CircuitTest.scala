package yuck.constraints.test

import org.junit._

import scala.collection._
import scala.jdk.CollectionConverters._

import yuck.annealing.DefaultMoveSizeDistribution
import yuck.constraints.test.util.ConstraintTestTooling
import yuck.constraints.{Circuit, CircuitNeighbourhood, CircuitTracker}
import yuck.core._
import yuck.test.util.UnitTest

/**
 * @author Michael Marte
 *
 */
@FixMethodOrder(runners.MethodSorters.NAME_ASCENDING)
@runner.RunWith(classOf[runners.Parameterized])
final class CircuitTest(offset: Int) extends UnitTest with ConstraintTestTooling {

    private val InvalidIndex1 = IntegerValue(offset - 1)
    private val One = IntegerValue(offset)
    private val Two = IntegerValue(offset + 1)
    private val Three = IntegerValue(offset + 2)
    private val Four = IntegerValue(offset + 3)
    private val Five = IntegerValue(offset + 4)
    private val InvalidIndex2 = IntegerValue(offset + 5)

    private val validIndexRange = IntegerRange(One, Five)

    private val randomGenerator = new JavaRandomGenerator
    private val space = new Space(logger, sigint)
    private val now = space.searchState

    private val baseDomain = IntegerRange(InvalidIndex1, InvalidIndex2)
    private val succ = for (i <- 1 to 5) yield new IntegerVariable(space.nextVariableId, "x%d".format(i), baseDomain)
    private val IndexedSeq(x1, x2, x3, x4, x5) = succ
    private val costs = new BooleanVariable(space.nextVariableId, "costs", CompleteBooleanDomain)

    @Test
    def testBasics: Unit = {
        val constraint = new Circuit(space.nextConstraintId, null, succ, offset, costs)
        assertEq(constraint.toString, "circuit([%s], %d, %s)".format(succ.mkString(", "), offset, costs))
        assertEq(constraint.inVariables.size, succ.size)
        assertEq(constraint.inVariables.toSet, succ.toSet)
        assertEq(constraint.outVariables.size, 1)
        assertEq(constraint.outVariables.head, costs)
    }

    @Test
    def testPropagation: Unit = {
        space.post(new Circuit(space.nextConstraintId, null, succ, offset, costs))
        runScenario(
            TestScenario(
                space,
                Propagate(
                    "Do not propagate 1",
                    Nil,
                    succ.map((_, baseDomain))),
                PropagateAndRollback(
                    "Do not propagate 2",
                    List((costs, FalseDomain)),
                    succ.map((_, baseDomain))),
                Propagate(
                    "Root-node propagation",
                    List((costs, TrueDomain)),
                    succ.indices.map(i => (succ(i), validIndexRange.diff(IntegerDomain(List(IntegerValue(offset + i))))))),
                Propagate(
                    "Propagate fixed node reference",
                    List((x1, IntegerDomain(List(Three)))),
                    succ.indices.map(i =>
                        (succ(i),
                         if (i == 0) IntegerDomain(List(Three))
                         else validIndexRange.diff(IntegerDomain(List(IntegerValue(offset + i), Three))))))))
    }

    @Test
    def testCostComputation: Unit = {
        space.post(new Circuit(space.nextConstraintId, null, succ, offset, costs))
        runScenario(
            TestScenario(
                space,
                Initialize("1 -> 2 -> 3 -> 4 -> 5 -> 1 (circuit)", (x1, Two), (x2, Three), (x3, Four), (x4, Five), (x5, One), (costs, True)),
                Initialize("1 -> 3 -> 5 -> 2 -> 4 -> 1 (circuit)", (x1, Three), (x2, Four), (x3, Five), (x4, One), (x5, Two), (costs, True)),
                Initialize("1 -> 2 -> 3 -> 4 -> 3, 5 -> 4 (1 subcircuit, 2 converging arcs)", (x1, Two), (x2, Three), (x3, Four), (x4, Three), (x5, Four), (costs, False3)),
                Initialize("1 -> 2 -> 1, 3 -> 4 -> 5 -> 3 (2 subcircuits)", (x1, Two), (x2, One), (x3, Four), (x4, Five), (x5, Three), (costs, False2)),
                Initialize("1 -> 1, 2 -> 1, 3 -> 1, 4 -> 1, 5 -> 1 (1 subcircuit, 5 converging arcs)", (x1, One), (x2, One), (x3, One), (x4, One), (x5, One), (costs, False4)),
                Initialize("1 -> 1, 2 -> 2, 3 -> 3, 4 -> 4, 5 -> 5 (5 subcircuits)", (x1, One), (x2, Two), (x3, Three), (x4, Four), (x5, Five), (costs, False4)),
                Consult("1 -> 2 -> 2, 3 -> 3, 4 -> 4, 5 -> 5 (4 subcircuits, 2 converging arcs)", (x1, Two), (costs, False4)),
                Consult("1 -> 1, 2 -> 2, 3 -> 4 -> 3, 5 -> 5 (4 subcircuits)", (x3, Four), (x4, Three), (costs, False3)),
                Consult("1 -> 3 -> 5 -> 2 -> 4 -> 1 (circuit)", (x1, Three), (x2, Four), (x3, Five), (x4, One), (x5, Two), (costs, True)),
                ConsultAndCommit("1 -> 1, 2 -> 3 -> 3, 4 -> 4, 5 -> 5 (3 subcircuits, 2 converging arcs)", (x2, Three), (costs, False4)),
                ConsultAndCommit("1 -> 1, 2 -> 3 -> 2, 4 -> 4, 5 -> 5 (3 subcircuits)", (x3, Two), (costs, False3)),
                ConsultAndCommit("1 -> 2 -> 3 -> 2, 4 -> 4, 5 -> 5 (3 subcircuits, 2 converging arcs)", (x1, Two), (costs, False3)),
                ConsultAndCommit("1 -> 2 -> 3 -> 2, 4 -> 5 -> 5 (2 subcircuits, 2 + 2 converging arcs)", (x4, Five), (costs, False3)),
                ConsultAndCommit("1 -> 2 -> 3 -> 2, 4 -> 5 -> 1 (1 subcircuit, 2 converging arcs)", (x5, One), (costs, False3)),
                ConsultAndCommit("1 -> 2 -> 3 -> 4 -> 5 -> 1 (circuit)", (x3, Four), (costs, True)),
                ConsultAndCommit("1 -> 2 -> 3 -> 4 -> 5 -> 2 (1 subcircuit, 2 converging arcs)", (x5, Two), (costs, False)),
                ConsultAndCommit("1 -> 2 -> 4 -> 5 -> 2, 3 -> 4 (1 subcircuit, 2 converging arcs)", (x2, Four), (costs, False2)),
                ConsultAndCommit("1 -> 2 -> 3 -> 4 -> 5 -> 1 (circuit)", (x2, Three), (x5, One), (costs, True)),
                ConsultAndCommit("1 -> 3 -> 5 -> 2 -> 4 -> 1 (circuit)", (x1, Three), (x2, Four), (x3, Five), (x4, One), (x5, Two), (costs, True))))
    }

    @Test
    def testHandlingOfDuplicateVariablesInCostComputation: Unit = {
        space.post(new Circuit(space.nextConstraintId, null, Vector(x1, x2, x3, x4, x1), offset, costs))
        runScenario(
            TestScenario(
                space,
                Initialize("1 -> 2 -> 3 -> 4 -> 5 -> 2", (x1, Two), (x2, Three), (x3, Four), (x4, Five), (costs, False)),
                Initialize("5 -> 1 -> 1, 2 -> 3 -> 4 -> 4", (x1, One), (x2, Three), (x3, Four), (x4, Four), (costs, False4)),
                Consult("1 -> 3, 2 -> 3 -> 4 -> 4, 5 -> 3", (x1, Three), (costs, False4)),
                Consult("5 -> 1 -> 1, 2 -> 3 -> 4 -> 2", (x4, Two), (costs, False2)),
                ConsultAndCommit("1 -> 3 -> 4 -> 4, 2 -> 3, 5 -> 2", (x1, Three), (costs, False4)),
                ConsultAndCommit("1 -> 3 -> 4 -> 2 -> 3, 5 -> 2", (x4, Two), (costs, False2))))
    }

    @Test
    def testHandlingOfInvalidNodeReferencesInCostComputation: Unit = {
        val constraint = new Circuit(space.nextConstraintId, null, succ, offset, costs)
        space.post(constraint)
        runScenario(
            TestScenario(
                space,
                Initialize("1 -> ?, 2 -> ?, 3 -> 4 -> 5 -> 1", (x1, InvalidIndex1), (x2, InvalidIndex2), (x3, Four), (x4, Five), (x5, Three), (costs, False2)),
                Initialize("1 -> 2 -> 1, 3 -> 4 -> 5 -> 3", (x1, Two), (x2, One), (x3, Four), (x4, Five), (x5, Three), (costs, False2)),
                Consult("1 -> 2 -> ?, 3 -> 4 -> 5 -> 3", (x2, InvalidIndex1), (costs, False2)),
                Consult("1 -> 2 -> 1, 3 -> 4 -> 5 -> ?", (x5, InvalidIndex2), (costs, False3)),
                ConsultAndCommit("1 -> 2 -> ?, 3 -> 4 -> 5 -> 3", (x2, InvalidIndex1), (costs, False2)),
                ConsultAndCommit("1 -> 2 -> ?, 3 -> 4 -> 5 -> ?", (x5, InvalidIndex2), (costs, False5))))
    }

    @Test
    def testNeighbourhoodGeneration: Unit = {
        val constraint = new Circuit(space.nextConstraintId, null, succ, offset, costs)
        space.post(constraint)
        assert(constraint.isCandidateForImplicitSolving(space))
        val neighbourhood =
            constraint.createNeighbourhood(space, randomGenerator, DefaultMoveSizeDistribution, logger, sigint).get
        assertEq(neighbourhood.getClass, classOf[CircuitNeighbourhood])
        assert(succ.forall(x => x.domain.contains(now.value(x))))
        assert(CircuitTracker.isHamiltonianCircuit(succ, offset, now))
        assertEq(now.value(costs), True)
        assertEq(neighbourhood.searchVariables, succ.toSet)
    }

    @Test
    def testHandlingOfDuplicateVariablesInNeighbourhoodGeneration: Unit = {
        val constraint = new Circuit(space.nextConstraintId, null, Vector(x1, x2, x3, x4, x1), offset, costs)
        space.post(constraint)
        assert(! constraint.isCandidateForImplicitSolving(space))
        assert(constraint.createNeighbourhood(space, randomGenerator, DefaultMoveSizeDistribution, logger, sigint).isEmpty)
    }

    @Test
    def testHandlingOfChannelVariablesInNeighbourhoodGeneration: Unit = {
        import yuck.constraints.Plus
        val constraint = new Circuit(space.nextConstraintId, null, succ, offset, costs)
        space.post(constraint)
        space.post(new Plus(space.nextConstraintId, null, x1, x2, x3))
        assert(! constraint.isCandidateForImplicitSolving(space))
        assert(constraint.createNeighbourhood(space, randomGenerator, DefaultMoveSizeDistribution, logger, sigint).isEmpty)
    }

    @Test
    def testHandlingOfFixedArcsInNeighbourhoodGeneration: Unit = {
        val constraint = new Circuit(space.nextConstraintId, null, succ, offset, costs)
        space.post(constraint)
        x1.pruneDomain(IntegerRange(Three, Three))
        assert(constraint.isCandidateForImplicitSolving(space))
        val neighbourhood =
            constraint.createNeighbourhood(space, randomGenerator, DefaultMoveSizeDistribution, logger, sigint).get
        assertEq(neighbourhood.getClass, classOf[CircuitNeighbourhood])
        assert(succ.forall(x => x.domain.contains(now.value(x))))
        assert(CircuitTracker.isHamiltonianCircuit(succ, offset, now))
        assertEq(now.value(costs), True)
        assertEq(neighbourhood.searchVariables, Set(x2, x3, x4, x5))
    }

    @Test
    def testHandlingOfConvergingArcsInNeighbourhoodGeneration: Unit = {
        val constraint = new Circuit(space.nextConstraintId, null, succ, offset, costs)
        space.post(constraint)
        x1.pruneDomain(IntegerRange(Three, Three))
        x2.pruneDomain(IntegerRange(Three, Three))
        assert(constraint.isCandidateForImplicitSolving(space))
        assert(constraint.createNeighbourhood(space, randomGenerator, DefaultMoveSizeDistribution, logger, sigint).isEmpty)
    }

    @Test
    def testHandlingOfSubcircuitsInNeighbourhoodGeneration: Unit = {
        val constraint = new Circuit(space.nextConstraintId, null, succ, offset, costs)
        space.post(constraint)
        x1.pruneDomain(IntegerRange(Two, Two))
        x2.pruneDomain(IntegerRange(Three, Three))
        x3.pruneDomain(IntegerRange(One, One))
        assert(constraint.isCandidateForImplicitSolving(space))
        assert(constraint.createNeighbourhood(space, randomGenerator, DefaultMoveSizeDistribution, logger, sigint).isEmpty)
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
