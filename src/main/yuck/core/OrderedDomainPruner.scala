package yuck.core

/**
 * Domain pruner interface for use by generic constraints.
 */
abstract class OrderedDomainPruner[A <: OrderedValue[A], D <: OrderedDomain[A, D]] extends DomainPruner[A, D] {

    override protected val typeTraits: OrderedTypeTraits[A, D, ?]

    def ltRule(lhs: D, rhs: D): (D, D) = (lhs, rhs)

    def leRule(lhs: D, rhs: D): (D, D) = (lhs, rhs)

    def minRule(lhs: Iterable[D], rhs: D): (Iterator[D], D) = (lhs.iterator, rhs)

    def maxRule(lhs: Iterable[D], rhs: D): (Iterator[D], D) = (lhs.iterator, rhs)

}
