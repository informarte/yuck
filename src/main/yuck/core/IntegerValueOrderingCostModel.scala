package yuck.core

/**
 * Implements the cost model for ordering operations on integer values.
 *
 * @author Michael Marte
 */
object IntegerValueOrderingCostModel extends OrderingCostModel[IntegerValue] {
    override def eqViolation(lhs: IntegerValue, rhs: IntegerValue) =
        abs(safeSub(lhs.value, rhs.value))
    override def neViolation(lhs: IntegerValue, rhs: IntegerValue) =
        if (lhs != rhs) 0 else 1
    override def ltViolation(lhs: IntegerValue, rhs: IntegerValue) =
        if (lhs < rhs) 0 else safeInc(safeSub(lhs.value, rhs.value))
    override def leViolation(lhs: IntegerValue, rhs: IntegerValue) =
        if (lhs <= rhs) 0 else safeSub(lhs.value, rhs.value)
}
