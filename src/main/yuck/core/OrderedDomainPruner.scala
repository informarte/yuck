package yuck.core

/**
 * OrderedDomain[V] pruner interface for use by generic constraints.
 *
 * @author Michael Marte
 */
abstract class OrderedDomainPruner[V <: OrderedValue[V]] extends DomainPruner[V] {

    override protected val valueTraits: OrderedValueTraits[V]

    override def eqRule
        (lhs: Domain[V], rhs: Domain[V]):
        (OrderedDomain[V], OrderedDomain[V]) =
        (valueTraits.safeDowncast(lhs), valueTraits.safeDowncast(rhs))

    override def neRule
        (lhs: Domain[V], rhs: Domain[V]):
        (OrderedDomain[V], OrderedDomain[V]) =
        (valueTraits.safeDowncast(lhs), valueTraits.safeDowncast(rhs))

    def ltRule
        (lhs: OrderedDomain[V], rhs: OrderedDomain[V]):
        (OrderedDomain[V], OrderedDomain[V]) =
        (lhs, rhs)

    def leRule
        (lhs: OrderedDomain[V], rhs: OrderedDomain[V]):
        (OrderedDomain[V], OrderedDomain[V]) =
        (lhs, rhs)

    def minRule
        (lhs: Iterable[OrderedDomain[V]], rhs: OrderedDomain[V]):
        (Iterator[OrderedDomain[V]], OrderedDomain[V]) =
        (lhs.iterator, rhs)

    def maxRule
        (lhs: Iterable[OrderedDomain[V]], rhs: OrderedDomain[V]):
        (Iterator[OrderedDomain[V]], OrderedDomain[V]) =
        (lhs.iterator, rhs)

}
