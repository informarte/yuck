package yuck.constraints.test

import yuck.constraints.{Inverse, InverseFunction, InverseNeighbourhood}
import yuck.core.*

abstract class InverseNeighbourhoodTest extends SpecificNeighbourhoodTest {

    protected val f: InverseFunction
    protected val g: InverseFunction

    override protected lazy val constraint =
        new Inverse(space.nextConstraintId(), f, g, costs, logger)

    override protected val expectedNeighbourhoodClass: Class[? <: InverseNeighbourhood]

    final override protected def checkSearchState(searchState: SearchState) = {
        assert(f.xs.forall(_.hasValidValue(searchState)))
        assert(g.xs.forall(_.hasValidValue(searchState)))
        assert(Inverse.areInverseFunctionsOfEachOther(f, g, searchState))
        assertEq(searchState.value(costs), True)
    }

}
