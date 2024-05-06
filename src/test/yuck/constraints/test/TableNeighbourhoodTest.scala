package yuck.constraints.test

import yuck.constraints.{Table, TableNeighbourhood}
import yuck.core.{*, given}

/**
 * @author Michael Marte
 *
 */
final class TableNeighbourhoodTest extends SpecialNeighbourhoodTest {

    private val xs = Vector("s", "t").map(new IntegerVariable(space.nextVariableId(), _, IntegerRange(0, 9)))

    private def createTable(m: Int)(elems: Int*) =
        elems.toVector.map(IntegerValue.apply).grouped(m).toVector
    private val rows = createTable(2)(0, 1, 0, 7, 1, 1, 1, 5, 3, 3, 5, 1, 5, 9, 8, 2, 8, 3, 9, 9)

    override protected def createConstraint() = {
        new Table(space.nextConstraintId(), null, xs, rows, costs)
    }

    override protected val expectedNeighbourhoodClass = classOf[TableNeighbourhood[?]]

    override protected def checkSearchState(searchState: SearchState) = {
        assert(xs.forall(_.hasValidValue(searchState)))
        assert(rows.contains(xs.map(searchState.value(_))))
        assertEq(searchState.value(costs), True)
    }

}
