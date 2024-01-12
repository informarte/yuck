package yuck.constraints.test

import yuck.constraints.{BooleanIncreasing, BooleanIncreasingNeighbourhood}
import yuck.core.{given, *}

/**
 * @author Michael Marte
 *
 */
final class BooleanIncreasingNeighbourhoodTest extends SpecialNeighbourhoodTest {

    private val numberOfVariables = 100
    private val xs = for (i <- 0 until numberOfVariables) yield {
        new BooleanVariable(space.nextVariableId(), "x%d".format(i + 1), CompleteBooleanDomain)
    }

    override protected def createConstraint() =
        new BooleanIncreasing(space.nextConstraintId(), null, xs, costs)

    override protected def checkSearchState(searchState: SearchState): Unit = {
        assert(xs.forall(_.hasValidValue(searchState)))
        for (i <- 0 until xs.size - 1) {
            assertGe(searchState.value(xs(i)), searchState.value(xs(i + 1)))
        }
        assertEq(searchState.value(costs), True)
    }

    override protected val expectedNeighbourhoodClass = classOf[BooleanIncreasingNeighbourhood]

}
