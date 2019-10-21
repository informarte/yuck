package yuck.core

/**
 * NumericalDomain[Value] pruner interface for use by generic constraints.
 *
 * @author Michael Marte
 */
abstract class NumericalDomainPruner[Value <: NumericalValue[Value]] extends OrderedDomainPruner[Value] {

    override protected val valueTraits: NumericalValueTraits[Value]

    override def eq
        (lhs: Domain[Value], rhs: Domain[Value]):
        (NumericalDomain[Value], NumericalDomain[Value]) =
        (valueTraits.safeDowncast(lhs), valueTraits.safeDowncast(rhs))

    override def ne
        (lhs: Domain[Value], rhs: Domain[Value]):
        (NumericalDomain[Value], NumericalDomain[Value]) =
        (valueTraits.safeDowncast(lhs), valueTraits.safeDowncast(rhs))

    override def lt
        (lhs: OrderedDomain[Value], rhs: OrderedDomain[Value]):
        (NumericalDomain[Value], NumericalDomain[Value]) =
        (valueTraits.safeDowncast(lhs), valueTraits.safeDowncast(rhs))

    override def le
        (lhs: OrderedDomain[Value], rhs: OrderedDomain[Value]):
        (NumericalDomain[Value], NumericalDomain[Value]) =
        (valueTraits.safeDowncast(lhs), valueTraits.safeDowncast(rhs))

    override def min
        (lhs: Iterable[OrderedDomain[Value]], rhs: OrderedDomain[Value]):
        (Iterator[NumericalDomain[Value]], NumericalDomain[Value]) =
        (lhs.iterator.map(valueTraits.safeDowncast), valueTraits.safeDowncast(rhs))

    override def max
        (lhs: Iterable[OrderedDomain[Value]], rhs: OrderedDomain[Value]):
        (Iterator[NumericalDomain[Value]], NumericalDomain[Value]) =
        (lhs.iterator.map(valueTraits.safeDowncast), valueTraits.safeDowncast(rhs))

    def linEq
        (lhs: Iterable[(Value, NumericalDomain[Value])], rhs: NumericalDomain[Value]):
        (Iterator[NumericalDomain[Value]], NumericalDomain[Value]) =
        (lhs.iterator.map(_._2), rhs)

    def times
        (dx: NumericalDomain[Value], dy: NumericalDomain[Value], dz: NumericalDomain[Value]):
        (NumericalDomain[Value], NumericalDomain[Value], NumericalDomain[Value])  =
        (dx, dy, dz)

}
