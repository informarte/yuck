package yuck.core

/**
 * Provides a cost model for ordering operations.
 *
 * @author Michael Marte
 */
abstract class OrderingCostModel[Value <: OrderedValue[Value]] {

    /** Basis for implementing equality constraints. */
    def eqViolation(lhs: Value, rhs: Value): Long = if (lhs == rhs) 0 else 1

    /** Basis for implementing inequality constraints. */
    def neViolation(lhs: Value, rhs: Value): Long = if (lhs != rhs) 0 else 1

    /** Basis for implementing less-than constraints. */
    def ltViolation(lhs: Value, rhs: Value): Long = if (lhs < rhs) 0 else 1

    /** Basis for implementing less-than-or-equal constraints. */
    def leViolation(lhs: Value, rhs: Value): Long = if (lhs <= rhs) 0 else 1

}
