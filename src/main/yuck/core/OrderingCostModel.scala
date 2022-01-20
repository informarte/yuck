package yuck.core

/**
 * Provides a cost model for ordering operations.
 *
 * @author Michael Marte
 */
abstract class OrderingCostModel[V <: OrderedValue[V]] {

    /** Basis for implementing equality constraints. */
    def eqViolation(lhs: V, rhs: V): Long = if (lhs == rhs) 0 else 1

    /** Basis for implementing inequality constraints. */
    def neViolation(lhs: V, rhs: V): Long = if (lhs != rhs) 0 else 1

    /** Basis for implementing less-than constraints. */
    def ltViolation(lhs: V, rhs: V): Long = if (lhs < rhs) 0 else 1

    /** Basis for implementing less-than-or-equal constraints. */
    def leViolation(lhs: V, rhs: V): Long = if (lhs <= rhs) 0 else 1

}
