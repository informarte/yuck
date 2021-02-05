package yuck.core

/**
 * Implements the cost model for ordering operations on integer values.
 *
 * @author Michael Marte
 */
object IntegerValueOrderingCostModel extends OrderingCostModel[IntegerValue] {
    override def eqViolation(lhs: IntegerValue, rhs: IntegerValue) =
        BooleanValue(abs(safeSub(lhs.value, rhs.value)))
    override def neViolation(lhs: IntegerValue, rhs: IntegerValue) =
        if (lhs != rhs) True else False
    override def ltViolation(lhs: IntegerValue, rhs: IntegerValue) =
        if (lhs < rhs) True else BooleanValue(safeInc(safeSub(lhs.value, rhs.value)))
    override def leViolation(lhs: IntegerValue, rhs: IntegerValue) =
        if (lhs <= rhs) True else BooleanValue(safeSub(lhs.value, rhs.value))
}
