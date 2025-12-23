package yuck.core

/**
 * Provides a cost model for equality and inequality constraints.
 */
abstract class EqualityCostModel[A <: Value[A]] {

    /** Basis for implementing equality constraints. */
    def eqViolation(lhs: A, rhs: A): Long = if lhs == rhs then 0 else 1

    /** Basis for implementing inequality constraints. */
    def neViolation(lhs: A, rhs: A): Long = if lhs != rhs then 0 else 1

}
