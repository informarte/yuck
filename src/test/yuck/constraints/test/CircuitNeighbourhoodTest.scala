package yuck.constraints.test

import org.junit.*

import scala.jdk.CollectionConverters.*

import yuck.constraints.{Circuit, CircuitNeighbourhood, CircuitTracker}
import yuck.core.{given, *}

/**
 * @author Michael Marte
 *
 */
@runner.RunWith(classOf[runners.Parameterized])
final class CircuitNeighbourhoodTest(offset: Int) extends SpecialNeighbourhoodTest {

    private val NumberOfNodes = 10

    private val baseDomain = IntegerRange(offset, offset + NumberOfNodes - 1)
    private val succ =
        for (i <- 1 to NumberOfNodes) yield
            new IntegerVariable(space.nextVariableId(), "x%d".format(i), baseDomain.randomSubdomain(randomGenerator))

    override protected def createConstraint() = {
        new Circuit(space.nextConstraintId(), null, succ, offset, costs, logger, sigint)
    }

    override protected val expectedNeighbourhoodClass = classOf[CircuitNeighbourhood]

    override protected def checkSearchState(searchState: SearchState) = {
        assert(succ.forall(_.hasValidValue(searchState)))
        assert(Circuit.isHamiltonianCircuit(succ, offset, searchState))
        assertEq(searchState.value(costs), True)
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
