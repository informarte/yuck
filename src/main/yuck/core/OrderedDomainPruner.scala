package yuck.core

/**
 * OrderedDomain[Value] pruner interface for use by generic constraints.
 *
 * @author Michael Marte
 */
abstract class OrderedDomainPruner[Value <: OrderedValue[Value]] extends DomainPruner[Value] {

    override protected val valueTraits: OrderedValueTraits[Value]

    override def eqRule
        (lhs: Domain[Value], rhs: Domain[Value]):
        (OrderedDomain[Value], OrderedDomain[Value]) =
        (valueTraits.safeDowncast(lhs), valueTraits.safeDowncast(rhs))

    override def neRule
        (lhs: Domain[Value], rhs: Domain[Value]):
        (OrderedDomain[Value], OrderedDomain[Value]) =
        (valueTraits.safeDowncast(lhs), valueTraits.safeDowncast(rhs))

    def ltRule
        (lhs: OrderedDomain[Value], rhs: OrderedDomain[Value]):
        (OrderedDomain[Value], OrderedDomain[Value]) =
        (lhs, rhs)

    def leRule
        (lhs: OrderedDomain[Value], rhs: OrderedDomain[Value]):
        (OrderedDomain[Value], OrderedDomain[Value]) =
        (lhs, rhs)

    def minRule
        (lhs: Iterable[OrderedDomain[Value]], rhs: OrderedDomain[Value]):
        (Iterator[OrderedDomain[Value]], OrderedDomain[Value]) =
        (lhs.iterator, rhs)

    def maxRule
        (lhs: Iterable[OrderedDomain[Value]], rhs: OrderedDomain[Value]):
        (Iterator[OrderedDomain[Value]], OrderedDomain[Value]) =
        (lhs.iterator, rhs)

}
