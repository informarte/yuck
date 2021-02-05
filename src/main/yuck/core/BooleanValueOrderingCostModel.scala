package yuck.core

/**
 * Implements the cost model for ordering operations on Boolean values.
 *
 * @author Michael Marte
 */
object BooleanValueOrderingCostModel extends OrderingCostModel[BooleanValue] {
    override def eqViolation(lhs: BooleanValue, rhs: BooleanValue) =
        if (lhs.truthValue == rhs.truthValue) True
        else BooleanValue(safeInc(safeAdd(lhs.violation, rhs.violation)) / 2)
    override def neViolation(lhs: BooleanValue, rhs: BooleanValue) =
        if (lhs.truthValue && rhs.truthValue) False
        else if (! lhs.truthValue && ! rhs.truthValue) BooleanValue(safeAdd(lhs.violation, rhs.violation) / 2)
        else True
    override def ltViolation(lhs: BooleanValue, rhs: BooleanValue) =
        if (lhs.truthValue) BooleanValue(safeInc(rhs.violation)) else rhs
    override def leViolation(lhs: BooleanValue, rhs: BooleanValue) =
        if (lhs.truthValue) BooleanValue(safeInc(rhs.violation) / 2) else True
}
