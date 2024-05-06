package yuck.constraints.test

import yuck.constraints.{Inverse, InverseFunction, InverseNeighbourhood}
import yuck.core.{given, *}

/**
 * @author Michael Marte
 *
 */
abstract class InverseNeighbourhoodTest extends SpecialNeighbourhoodTest {

    protected val f: InverseFunction
    protected val g: InverseFunction

    final override protected def createConstraint() = {
        new Inverse(space.nextConstraintId(), null, f, g, costs, logger)
    }

    override protected val expectedNeighbourhoodClass: Class[? <: InverseNeighbourhood]

    final override protected def checkSearchState(searchState: SearchState) = {
        assert(f.xs.forall(_.hasValidValue(searchState)))
        assert(g.xs.forall(_.hasValidValue(searchState)))
        assert(Inverse.areInverseFunctionsOfEachOther(f, g, searchState))
        assertEq(searchState.value(costs), True)
    }

}
