package yuck.core

/**
 * OrderedDomain[Value] pruner interface for use by generic constraints.
 *
 * @author Michael Marte
 */
abstract class OrderedDomainPruner[Value <: OrderedValue[Value]] extends DomainPruner[Value] {

    override protected val valueTraits: OrderedValueTraits[Value]

    override def eq
        (lhs: Domain[Value], rhs: Domain[Value]):
        (OrderedDomain[Value], OrderedDomain[Value]) =
        (valueTraits.safeDowncast(lhs), valueTraits.safeDowncast(rhs))

    override def ne
        (lhs: Domain[Value], rhs: Domain[Value]):
        (OrderedDomain[Value], OrderedDomain[Value]) =
        (valueTraits.safeDowncast(lhs), valueTraits.safeDowncast(rhs))

    def lt
        (lhs: OrderedDomain[Value], rhs: OrderedDomain[Value]):
        (OrderedDomain[Value], OrderedDomain[Value]) =
        (lhs, rhs)

    def le
        (lhs: OrderedDomain[Value], rhs: OrderedDomain[Value]):
        (OrderedDomain[Value], OrderedDomain[Value]) =
        (lhs, rhs)

    def min
        (lhs: Iterable[OrderedDomain[Value]], rhs: OrderedDomain[Value]):
        (Iterator[OrderedDomain[Value]], OrderedDomain[Value]) =
        (lhs.iterator, rhs)

    def max
        (lhs: Iterable[OrderedDomain[Value]], rhs: OrderedDomain[Value]):
        (Iterator[OrderedDomain[Value]], OrderedDomain[Value]) =
        (lhs.iterator, rhs)

}
