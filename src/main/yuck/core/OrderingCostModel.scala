package yuck.core

/**
 * Provides a cost model for ordering constraints.
 */
abstract class OrderingCostModel[A <: OrderedValue[A]] extends EqualityCostModel[A] {

    /** Basis for implementing less-than constraints. */
    def ltViolation(lhs: A, rhs: A): Long = if lhs < rhs then 0 else 1

    /** Basis for implementing less-than-or-equal constraints. */
    def leViolation(lhs: A, rhs: A): Long = if lhs <= rhs then 0 else 1

}
