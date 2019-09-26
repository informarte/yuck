package yuck.core

/**
 * Implements the cost model for ordering operations on Boolean values.
 *
 * @author Michael Marte
 */
final object BooleanValueOrderingCostModel extends OrderingCostModel[BooleanValue] {
    override def eq(lhs: BooleanValue, rhs: BooleanValue) =
        if (lhs.truthValue == rhs.truthValue) True
        else BooleanValue.get(safeInc(safeAdd(lhs.violation, rhs.violation)) / 2)
    override def ne(lhs: BooleanValue, rhs: BooleanValue) =
        if (lhs.truthValue && rhs.truthValue) False
        else if (! lhs.truthValue && ! rhs.truthValue) BooleanValue.get(safeAdd(lhs.violation, rhs.violation) / 2)
        else True
    override def lt(lhs: BooleanValue, rhs: BooleanValue) =
        if (lhs.truthValue) BooleanValue.get(safeInc(rhs.violation)) else rhs
    override def le(lhs: BooleanValue, rhs: BooleanValue) =
        if (lhs.truthValue) BooleanValue.get(safeInc(rhs.violation) / 2) else True
}
