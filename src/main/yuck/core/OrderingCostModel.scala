package yuck.core

/**
 * Provides a cost model for ordering operations.
 *
 * @author Michael Marte
 */
abstract class OrderingCostModel[Value <: OrderedValue[Value]] {

    /** Basis for implementing equality constraints. */
    def eq(lhs: Value, rhs: Value): BooleanValue = if (lhs == rhs) True else False

    /** Basis for implementing inequality constraints. */
    def ne(lhs: Value, rhs: Value): BooleanValue = if (lhs != rhs) True else False

    /** Basis for implementing less-than constraints. */
    def lt(lhs: Value, rhs: Value): BooleanValue = if (lhs < rhs) True else False

    /** Basis for implementing less-than-or-equal constraints. */
    def le(lhs: Value, rhs: Value): BooleanValue = if (lhs <= rhs) True else False

}
