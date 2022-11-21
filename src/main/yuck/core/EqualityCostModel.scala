package yuck.core

/**
 * Provides a cost model for equality and inequality constraints.
 *
 * @author Michael Marte
 */
abstract class EqualityCostModel[V <: AnyValue] {

    /** Basis for implementing equality constraints. */
    def eqViolation(lhs: V, rhs: V): Long = if (lhs == rhs) 0 else 1

    /** Basis for implementing inequality constraints. */
    def neViolation(lhs: V, rhs: V): Long = if (lhs != rhs) 0 else 1

}
