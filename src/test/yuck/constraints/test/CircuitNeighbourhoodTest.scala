package yuck.constraints.test

import org.junit.*

import scala.jdk.CollectionConverters.*

import yuck.annealing.DefaultMoveSizeDistribution
import yuck.constraints.{Circuit, CircuitNeighbourhood, CircuitTracker}
import yuck.core.*
import yuck.test.util.UnitTest

/**
 * @author Michael Marte
 *
 */
@FixMethodOrder(runners.MethodSorters.NAME_ASCENDING)
@runner.RunWith(classOf[runners.Parameterized])
final class CircuitNeighbourhoodTest(offset: Int) extends UnitTest {

    private val randomGenerator = new JavaRandomGenerator
    private val space = new Space(logger, sigint)
    private val now = space.searchState

    private val numberOfNodes = 10
    private val baseDomain = IntegerRange(offset, offset + numberOfNodes - 1)
    private val succ =
        for (i <- 1 to numberOfNodes) yield
            new IntegerVariable(space.nextVariableId(), "x%d".format(i), baseDomain.randomSubdomain(randomGenerator))
    private val costs = new BooleanVariable(space.nextVariableId(), "costs", CompleteBooleanDomain)

    @Test
    def testMoveGeneration(): Unit = {
        val constraint = new Circuit(space.nextConstraintId(), null, succ, offset, costs)
        space.post(constraint)
        val neighbourhood =
            constraint.createNeighbourhood(space, randomGenerator, DefaultMoveSizeDistribution, logger, sigint).get
        assertEq(neighbourhood.getClass, classOf[CircuitNeighbourhood])
        assert(Circuit.isHamiltonianCircuit(succ, offset, now))
        assertEq(now.value(costs), True)
        space.initialize()
        val sampleSize = 1000
        for (i <- 1 to sampleSize) {
            val move = neighbourhood.nextMove
            val after = space.consult(move)
            assert(succ.exists(x => now.value(x) != after.value(x)))
            assert(Circuit.isHamiltonianCircuit(succ, offset, after))
            if (randomGenerator.nextDecision()) {
                space.commit(move)
                neighbourhood.commit(move)
            }
        }
    }

}

/**
 * @author Michael Marte
 *
 */
object CircuitNeighbourhoodTest {

    @runners.Parameterized.Parameters(name = "{index}: {0}")
    def parameters = List(-1, 0, 1).asJava

}
