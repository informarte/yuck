package yuck.core

/**
 * Implements the cost model for ordering operations on integer values.
 *
 * @author Michael Marte
 */
final object IntegerOrderingCostModel extends OrderingCostModel[IntegerValue] {
    override def eq(lhs: IntegerValue, rhs: IntegerValue) =
        BooleanValue.get(scala.math.abs(safeSub(lhs.value, rhs.value)))
    override def ne(lhs: IntegerValue, rhs: IntegerValue) =
        if (lhs != rhs) True else False
    override def lt(lhs: IntegerValue, rhs: IntegerValue) =
        if (lhs < rhs) True else BooleanValue.get(safeInc(safeSub(lhs.value, rhs.value)))
    override def le(lhs: IntegerValue, rhs: IntegerValue) =
        if (lhs <= rhs) True else BooleanValue.get(safeSub(lhs.value, rhs.value))
}
