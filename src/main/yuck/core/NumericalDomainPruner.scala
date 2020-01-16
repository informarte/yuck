package yuck.core

/**
 * NumericalDomain[Value] pruner interface for use by generic constraints.
 *
 * @author Michael Marte
 */
abstract class NumericalDomainPruner[Value <: NumericalValue[Value]] extends OrderedDomainPruner[Value] {

    override protected val valueTraits: NumericalValueTraits[Value]

    override def eqRule
        (lhs: Domain[Value], rhs: Domain[Value]):
        (NumericalDomain[Value], NumericalDomain[Value]) =
        (valueTraits.safeDowncast(lhs), valueTraits.safeDowncast(rhs))

    override def neRule
        (lhs: Domain[Value], rhs: Domain[Value]):
        (NumericalDomain[Value], NumericalDomain[Value]) =
        (valueTraits.safeDowncast(lhs), valueTraits.safeDowncast(rhs))

    override def ltRule
        (lhs: OrderedDomain[Value], rhs: OrderedDomain[Value]):
        (NumericalDomain[Value], NumericalDomain[Value]) =
        (valueTraits.safeDowncast(lhs), valueTraits.safeDowncast(rhs))

    override def leRule
        (lhs: OrderedDomain[Value], rhs: OrderedDomain[Value]):
        (NumericalDomain[Value], NumericalDomain[Value]) =
        (valueTraits.safeDowncast(lhs), valueTraits.safeDowncast(rhs))

    override def minRule
        (lhs: Iterable[OrderedDomain[Value]], rhs: OrderedDomain[Value]):
        (Iterator[NumericalDomain[Value]], NumericalDomain[Value]) =
        (lhs.iterator.map(valueTraits.safeDowncast), valueTraits.safeDowncast(rhs))

    override def maxRule
        (lhs: Iterable[OrderedDomain[Value]], rhs: OrderedDomain[Value]):
        (Iterator[NumericalDomain[Value]], NumericalDomain[Value]) =
        (lhs.iterator.map(valueTraits.safeDowncast), valueTraits.safeDowncast(rhs))

    def absRule
        (lhs: NumericalDomain[Value], rhs: NumericalDomain[Value]):
        (NumericalDomain[Value], NumericalDomain[Value]) =
        (lhs, rhs)

    def linEqRule
        (lhs: Iterable[(Value, NumericalDomain[Value])], rhs: NumericalDomain[Value]):
        (Iterator[NumericalDomain[Value]], NumericalDomain[Value]) =
        (lhs.iterator.map(_._2), rhs)

    def timesRule
        (dx: NumericalDomain[Value], dy: NumericalDomain[Value], dz: NumericalDomain[Value]):
        (NumericalDomain[Value], NumericalDomain[Value], NumericalDomain[Value])  =
        (dx, dy, dz)

}
