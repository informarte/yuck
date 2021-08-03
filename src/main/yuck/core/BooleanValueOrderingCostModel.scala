package yuck.core

/**
 * Implements the cost model for ordering operations on Boolean values.
 *
 * @author Michael Marte
 */
object BooleanValueOrderingCostModel extends OrderingCostModel[BooleanValue] {
    override def eqViolation(lhs: BooleanValue, rhs: BooleanValue) =
        if (lhs.truthValue == rhs.truthValue) 0
        else safeInc(safeAdd(lhs.violation, rhs.violation)) / 2
    override def neViolation(lhs: BooleanValue, rhs: BooleanValue) =
        if (lhs.truthValue && rhs.truthValue) 1
        else if (! lhs.truthValue && ! rhs.truthValue) safeAdd(lhs.violation, rhs.violation) / 2
        else 0
    override def ltViolation(lhs: BooleanValue, rhs: BooleanValue) =
        if (lhs.truthValue) safeInc(rhs.violation) else rhs.violation
    override def leViolation(lhs: BooleanValue, rhs: BooleanValue) =
        if (lhs.truthValue) safeInc(rhs.violation) / 2 else 0
}
