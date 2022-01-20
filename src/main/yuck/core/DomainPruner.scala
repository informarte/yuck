package yuck.core

/**
 * Domain pruner interface for use by generic constraints.
 *
 * @author Michael Marte
 */
abstract class DomainPruner[V <: AnyValue] {

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
