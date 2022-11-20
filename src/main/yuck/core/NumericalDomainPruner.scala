package yuck.core

/**
 * Domain pruner interface for use by generic constraints.
 *
 * @author Michael Marte
 */
abstract class NumericalDomainPruner[V <: NumericalValue[V]] extends OrderedDomainPruner[V] {

    override protected val valueTraits: NumericalValueTraits[V]

    override def eqRule
        (lhs: Domain[V], rhs: Domain[V]):
        (NumericalDomain[V], NumericalDomain[V]) =
        (valueTraits.safeDowncast(lhs), valueTraits.safeDowncast(rhs))

    override def neRule
        (lhs: Domain[V], rhs: Domain[V]):
        (NumericalDomain[V], NumericalDomain[V]) =
        (valueTraits.safeDowncast(lhs), valueTraits.safeDowncast(rhs))

    override def ltRule
        (lhs: OrderedDomain[V], rhs: OrderedDomain[V]):
        (NumericalDomain[V], NumericalDomain[V]) =
        (valueTraits.safeDowncast(lhs), valueTraits.safeDowncast(rhs))

    override def leRule
        (lhs: OrderedDomain[V], rhs: OrderedDomain[V]):
        (NumericalDomain[V], NumericalDomain[V]) =
        (valueTraits.safeDowncast(lhs), valueTraits.safeDowncast(rhs))

    override def minRule
        (lhs: Iterable[OrderedDomain[V]], rhs: OrderedDomain[V]):
        (Iterator[NumericalDomain[V]], NumericalDomain[V]) =
        (lhs.iterator.map(valueTraits.safeDowncast), valueTraits.safeDowncast(rhs))

    override def maxRule
        (lhs: Iterable[OrderedDomain[V]], rhs: OrderedDomain[V]):
        (Iterator[NumericalDomain[V]], NumericalDomain[V]) =
        (lhs.iterator.map(valueTraits.safeDowncast), valueTraits.safeDowncast(rhs))

    def absRule
        (lhs: NumericalDomain[V], rhs: NumericalDomain[V]):
        (NumericalDomain[V], NumericalDomain[V]) =
        (lhs, rhs)

    def linEqRule
        (lhs: Iterable[(V, NumericalDomain[V])], rhs: NumericalDomain[V]):
        (Iterator[NumericalDomain[V]], NumericalDomain[V]) =
        (lhs.iterator.map(_._2), rhs)

    def timesRule
        (dx: NumericalDomain[V], dy: NumericalDomain[V], dz: NumericalDomain[V]):
        (NumericalDomain[V], NumericalDomain[V], NumericalDomain[V])  =
        (dx, dy, dz)

}
