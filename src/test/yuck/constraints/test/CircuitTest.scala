package yuck.constraints.test

import org.junit._

import scala.collection._
import scala.jdk.CollectionConverters._

import yuck.annealing.DefaultMoveSizeDistribution
import yuck.constraints.{Circuit, CircuitTracker, CircuitNeighbourhood}
import yuck.core._
import yuck.util.testing.UnitTest

/**
 * @author Michael Marte
 *
 */
@Test
@FixMethodOrder(runners.MethodSorters.NAME_ASCENDING)
@runner.RunWith(classOf[runners.Parameterized])
final class CircuitTest(offset: Int)
    extends UnitTest
    with CostComputationTestTooling[BooleanValue]
    with PropagationTestTooling
{

    private val InvalidIndex1 = IntegerValue.get(offset - 1)
    private val One = IntegerValue.get(offset)
    private val Two = IntegerValue.get(offset + 1)
    private val Three = IntegerValue.get(offset + 2)
    private val Four = IntegerValue.get(offset + 3)
    private val Five = IntegerValue.get(offset + 4)
    private val InvalidIndex2 = IntegerValue.get(offset + 5)

    private val randomGenerator = new JavaRandomGenerator
    private val space = new Space(logger, sigint)
    private val now = space.searchState

    private val baseDomain = new IntegerRange(InvalidIndex1, InvalidIndex2)
    private val succ @ IndexedSeq(x1, x2, x3, x4, x5) =
        for (i <- 1 to 5) yield new IntegerVariable(space.nextVariableId, "x%d".format(i), baseDomain)
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
            PropagationTestScenario(
                space,
                Propagate(
                    "Do not propagate 1", Nil, () => assert(succ.forall(_.domain == baseDomain))),
                PropagateAndRollback(
                    "Do not propagate 2", (costs, FalseDomain), () => assert(succ.forall(_.domain == baseDomain))),
                Propagate(
                    "Root-node propagation",
                    (costs, TrueDomain),
                    () => {
                        val validIndexRange = new IntegerRange(One, Five)
                        for (i <- 0 to 4) {
                            assert(succ(i).domain.isSubsetOf(validIndexRange))
                            assert(! succ(i).domain.contains(IntegerValue.get(offset + i)))
                        }
                    }
                ),
                Propagate(
                    "Propagate fixed node reference",
                    (x1, new IntegerRange(Three, Three)),
                    () => {
                        for (i <- 1 to 4) {
                            assert(! succ(i).domain.contains(Three))
                        }
                    }
                )))
    }

    @Test
    def testCostComputation: Unit = {
        space.post(new Circuit(space.nextConstraintId, null, succ, offset, costs))
        runScenario(
            CostComputationTestScenario(
                space,
                costs,
                Initialize("1 -> 2 -> 3 -> 4 -> 5 -> 1 (circuit)", True, (x1, Two), (x2, Three), (x3, Four), (x4, Five), (x5, One)),
                Initialize("1 -> 3 -> 5 -> 2 -> 4 -> 1 (circuit)", True, (x1, Three), (x2, Four), (x3, Five), (x4, One), (x5, Two)),
                Initialize("1 -> 2 -> 3 -> 4 -> 3, 5 -> 4 (1 subcircuit, 2 converging arcs)", False3, (x1, Two), (x2, Three), (x3, Four), (x4, Three), (x5, Four)),
                Initialize("1 -> 2 -> 1, 3 -> 4 -> 5 -> 3 (2 subcircuits)", False2, (x1, Two), (x2, One), (x3, Four), (x4, Five), (x5, Three)),
                Initialize("1 -> 1, 2 -> 1, 3 -> 1, 4 -> 1, 5 -> 1 (1 subcircuit, 5 converging arcs)", False4, (x1, One), (x2, One), (x3, One), (x4, One), (x5, One)),
                Initialize("1 -> 1, 2 -> 2, 3 -> 3, 4 -> 4, 5 -> 5 (5 subcircuits)", False4, (x1, One), (x2, Two), (x3, Three), (x4, Four), (x5, Five)),
                Consult("1 -> 2 -> 2, 3 -> 3, 4 -> 4, 5 -> 5 (4 subcircuits, 2 converging arcs)", False4, (x1, Two)),
                Consult("1 -> 1, 2 -> 2, 3 -> 4 -> 3, 5 -> 5 (4 subcircuits)", False3, (x3, Four), (x4, Three)),
                Consult("1 -> 3 -> 5 -> 2 -> 4 -> 1 (circuit)", True, (x1, Three), (x2, Four), (x3, Five), (x4, One), (x5, Two)),
                ConsultAndCommit("1 -> 1, 2 -> 3 -> 3, 4 -> 4, 5 -> 5 (3 subcircuits, 2 converging arcs)", False4, (x2, Three)),
                ConsultAndCommit("1 -> 1, 2 -> 3 -> 2, 4 -> 4, 5 -> 5 (3 subcircuits)", False3, (x3, Two)),
                ConsultAndCommit("1 -> 2 -> 3 -> 2, 4 -> 4, 5 -> 5 (3 subcircuits, 2 converging arcs)", False3, (x1, Two)),
                ConsultAndCommit("1 -> 2 -> 3 -> 2, 4 -> 5 -> 5 (2 subcircuits, 2 + 2 converging arcs)", False3, (x4, Five)),
                ConsultAndCommit("1 -> 2 -> 3 -> 2, 4 -> 5 -> 1 (1 subcircuit, 2 converging arcs)", False3, (x5, One)),
                ConsultAndCommit("1 -> 2 -> 3 -> 4 -> 5 -> 1 (circuit)", True, (x3, Four)),
                ConsultAndCommit("1 -> 2 -> 3 -> 4 -> 5 -> 2 (1 subcircuit, 2 converging arcs)", False, (x5, Two)),
                ConsultAndCommit("1 -> 2 -> 4 -> 5 -> 2, 3 -> 4 (1 subcircuit, 2 converging arcs)", False2, (x2, Four)),
                ConsultAndCommit("1 -> 2 -> 3 -> 4 -> 5 -> 1 (circuit)", True, (x2, Three), (x5, One)),
                ConsultAndCommit("1 -> 3 -> 5 -> 2 -> 4 -> 1 (circuit)", True, (x1, Three), (x2, Four), (x3, Five), (x4, One), (x5, Two))))
    }

    @Test
    def testHandlingOfDuplicateVariablesInCostComputation: Unit = {
        space.post(new Circuit(space.nextConstraintId, null, Vector(x1, x2, x3, x4, x1), offset, costs))
        runScenario(
            CostComputationTestScenario(
                space,
                costs,
                Initialize("1 -> 2 -> 3 -> 4 -> 5 -> 2", False, (x1, Two), (x2, Three), (x3, Four), (x4, Five)),
                Initialize("5 -> 1 -> 1, 2 -> 3 -> 4 -> 4", False4, (x1, One), (x2, Three), (x3, Four), (x4, Four)),
                Consult("1 -> 3, 2 -> 3 -> 4 -> 4, 5 -> 3", False4, (x1, Three)),
                Consult("5 -> 1 -> 1, 2 -> 3 -> 4 -> 2", False2, (x4, Two)),
                ConsultAndCommit("1 -> 3 -> 4 -> 4, 2 -> 3, 5 -> 2", False4, (x1, Three)),
                ConsultAndCommit("1 -> 3 -> 4 -> 2 -> 3, 5 -> 2", False2, (x4, Two))))
    }

    @Test
    def testHandlingOfInvalidNodeReferencesInCostComputation: Unit = {
        val constraint = new Circuit(space.nextConstraintId, null, succ, offset, costs)
        space.post(constraint)
        runScenario(
            CostComputationTestScenario(
                space,
                costs,
                Initialize("1 -> ?, 2 -> ?, 3 -> 4 -> 5 -> 1", False2, (x1, InvalidIndex1), (x2, InvalidIndex2), (x3, Four), (x4, Five), (x5, Three)),
                Initialize("1 -> 2 -> 1, 3 -> 4 -> 5 -> 3", False2, (x1, Two), (x2, One), (x3, Four), (x4, Five), (x5, Three)),
                Consult("1 -> 2 -> ?, 3 -> 4 -> 5 -> 3", False2, (x2, InvalidIndex1)),
                Consult("1 -> 2 -> 1, 3 -> 4 -> 5 -> ?", False3, (x5, InvalidIndex2)),
                ConsultAndCommit("1 -> 2 -> ?, 3 -> 4 -> 5 -> 3", False2, (x2, InvalidIndex1)),
                ConsultAndCommit("1 -> 2 -> ?, 3 -> 4 -> 5 -> ?", False5, (x5, InvalidIndex2))))
    }

    @Test
    def testNeighbourhoodGeneration: Unit = {
        val constraint = new Circuit(space.nextConstraintId, null, succ, offset, costs)
        space.post(constraint)
        assert(constraint.isCandidateForImplicitSolving(space))
        val neighbourhood =
            constraint.createNeighbourhood(space, randomGenerator, DefaultMoveSizeDistribution, logger, sigint).get
        assertEq(neighbourhood.getClass, classOf[CircuitNeighbourhood])
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
        x1.pruneDomain(new IntegerRange(Three, Three))
        assert(constraint.isCandidateForImplicitSolving(space))
        val neighbourhood =
            constraint.createNeighbourhood(space, randomGenerator, DefaultMoveSizeDistribution, logger, sigint).get
        assertEq(neighbourhood.getClass, classOf[CircuitNeighbourhood])
        assert(CircuitTracker.isHamiltonianCircuit(succ, offset, now))
        assertEq(now.value(costs), True)
        assertEq(neighbourhood.searchVariables, Set(x2, x3, x4, x5))
    }

    @Test
    def testHandlingOfConvergingArcsInNeighbourhoodGeneration: Unit = {
        val constraint = new Circuit(space.nextConstraintId, null, succ, offset, costs)
        space.post(constraint)
        x1.pruneDomain(new IntegerRange(Three, Three))
        x2.pruneDomain(new IntegerRange(Three, Three))
        assert(constraint.isCandidateForImplicitSolving(space))
        assert(constraint.createNeighbourhood(space, randomGenerator, DefaultMoveSizeDistribution, logger, sigint).isEmpty)
    }

    @Test
    def testHandlingOfSubcircuitsInNeighbourhoodGeneration: Unit = {
        val constraint = new Circuit(space.nextConstraintId, null, succ, offset, costs)
        space.post(constraint)
        x1.pruneDomain(new IntegerRange(Two, Two))
        x2.pruneDomain(new IntegerRange(Three, Three))
        x3.pruneDomain(new IntegerRange(One, One))
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
