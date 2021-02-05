package yuck.constraints.test

import org.junit._

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
class CircuitNeighbourhoodTest(offset: Int) extends UnitTest {

    private val randomGenerator = new JavaRandomGenerator
    private val space = new Space(logger, sigint)
    private val now = space.searchState

    private val baseDomain = IntegerRange(IntegerValue(offset), IntegerValue(offset + 4))
    private val xs = for (i <- 1 to 5) yield new IntegerVariable(space.nextVariableId, "x%d".format(i), baseDomain)
    private val costs = new BooleanVariable(space.nextVariableId, "costs", CompleteBooleanDomain)

    @Test
    def testMoveGeneration: Unit = {
        val constraint = new Circuit(space.nextConstraintId, null, xs, offset, costs)
        space.post(constraint)
        val neighbourhood =
            constraint.createNeighbourhood(space, randomGenerator, DefaultMoveSizeDistribution, logger, sigint).get
        assertEq(neighbourhood.getClass, classOf[CircuitNeighbourhood])
        assert(CircuitTracker.isHamiltonianCircuit(xs, offset, now))
        assertEq(now.value(costs), True)
        space.initialize
        val sampleSize = 1000
        for (i <- 1 to sampleSize) {
            val move = neighbourhood.nextMove
            val after = space.consult(move)
            assert(xs.exists(x => now.value(x) != after.value(x)))
            assert(CircuitTracker.isHamiltonianCircuit(xs, offset, after))
            space.commit(move)
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
