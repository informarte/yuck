package yuck.constraints.test

import yuck.constraints.{Alldistinct, AlldistinctNeighbourhood}
import yuck.core.{given, *}

/**
 * @author Michael Marte
 *
 */
final class AlldistinctNeighbourhoodTest extends SpecialNeighbourhoodTest {

    private val numberOfVariables = 100
    private val xs = for (i <- 0 until numberOfVariables) yield {
        val dx = IntegerRange(1, 2 * numberOfVariables).diff(IntegerDomain(List(i)))
        new IntegerVariable(space.nextVariableId(), "x%d".format(i + 1), dx)
    }

    override protected def createConstraint() = {
        new Alldistinct(space.nextConstraintId(), null, xs, costs, logger)
    }

    override protected val expectedNeighbourhoodClass = classOf[AlldistinctNeighbourhood[_]]

    override protected def checkSearchState(searchState: SearchState) = {
        assert(xs.forall(_.hasValidValue(searchState)))
        assertEq(xs.map(searchState.value(_)).toSet.size, xs.size)
        assertEq(searchState.value(costs), True)
    }

}
