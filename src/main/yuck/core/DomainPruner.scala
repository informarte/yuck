package yuck.core

/**
 * Domain pruner interface for use by generic constraints.
 */
abstract class DomainPruner[A <: Value[A], D <: Domain[A, D]] {

    protected val typeTraits: TypeTraits[A, D, ?]

    def eqRule(lhs: D, rhs: D): (D, D) = (lhs, rhs)

    def neRule(lhs: D, rhs: D): (D, D) = (lhs, rhs)

}
