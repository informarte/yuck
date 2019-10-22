package yuck.core

/**
 * Implements the cost model for ordering operations on integer values.
 *
 * @author Michael Marte
 */
object IntegerValueOrderingCostModel extends OrderingCostModel[IntegerValue] {
    override def eqViolation(lhs: IntegerValue, rhs: IntegerValue) =
        BooleanValue.get(abs(safeSub(lhs.value, rhs.value)))
    override def neViolation(lhs: IntegerValue, rhs: IntegerValue) =
        if (lhs != rhs) True else False
    override def ltViolation(lhs: IntegerValue, rhs: IntegerValue) =
        if (lhs < rhs) True else BooleanValue.get(safeInc(safeSub(lhs.value, rhs.value)))
    override def leViolation(lhs: IntegerValue, rhs: IntegerValue) =
        if (lhs <= rhs) True else BooleanValue.get(safeSub(lhs.value, rhs.value))
}
