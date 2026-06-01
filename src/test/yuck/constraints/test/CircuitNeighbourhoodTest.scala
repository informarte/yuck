package yuck.constraints.test

import org.junit.jupiter.params.ParameterizedClass
import org.junit.jupiter.params.provider.MethodSource

import yuck.constraints.{Circuit, CircuitNeighbourhood}
import yuck.core.*

@ParameterizedClass
@MethodSource(Array("parameters"))
final class CircuitNeighbourhoodTest(offset: Int) extends SpecificNeighbourhoodTest {

    private val numberOfNodes = 10
    private val baseDomain = IntegerRange(offset, offset + numberOfNodes - 1)
    private val succ =
        for i <- 1 to numberOfNodes yield
            new IntegerVariable(space.nextVariableId(), "x%d".format(i), baseDomain.randomSubdomain(randomGenerator))

    override protected lazy val constraint =
        new Circuit(space.nextConstraintId(), succ, offset, costs)

    override protected val expectedNeighbourhoodClass = classOf[CircuitNeighbourhood]

    override protected def checkSearchState(searchState: SearchState) = {
        assert(succ.forall(_.hasValidValue(searchState)))
        assert(Circuit.isHamiltonianCircuit(succ, offset, searchState))
        assertEq(searchState.value(costs), True)
    }

}

object CircuitNeighbourhoodTest {

    def parameters = Array(-1, 0, 1).map(Int.box)

}
