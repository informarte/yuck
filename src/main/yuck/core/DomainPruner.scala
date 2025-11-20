package yuck.core

/**
 * Domain pruner interface for use by generic constraints.
 */
abstract class DomainPruner[V <: Value[V]] {

    protected val valueTraits: ValueTraits[V]

    def eqRule
        (lhs: Domain[V], rhs: Domain[V]):
        (Domain[V], Domain[V]) =
        (lhs, rhs)

    def neRule
        (lhs: Domain[V], rhs: Domain[V]):
        (Domain[V], Domain[V]) =
        (lhs, rhs)

}
