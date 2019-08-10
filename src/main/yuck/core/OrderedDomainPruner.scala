package yuck.core

/**
 * Domain pruner interface for use by generic constraints.
 *
 * @author Michael Marte
 */
abstract class OrderedDomainPruner[Value <: OrderedValue[Value]] extends DomainPruner[Value] {

    type DomainImpl <: OrderedDomain[Value]

    def lt
        [Domain >: DomainImpl <: OrderedDomain[Value]]
        (lhs: Domain, rhs: Domain):
        (Domain, Domain) =
        (lhs, rhs)

    def le
        [Domain >: DomainImpl <: OrderedDomain[Value]]
        (lhs: Domain, rhs: Domain):
        (Domain, Domain) =
        (lhs, rhs)

    def min
        [Domain >: DomainImpl <: OrderedDomain[Value]]
        (lhs: Iterable[Domain], rhs: Domain):
        (Iterator[Domain], Domain) =
        (lhs.iterator, rhs)

    def max
        [Domain >: DomainImpl <: OrderedDomain[Value]]
        (lhs: Iterable[Domain], rhs: Domain):
        (Iterator[Domain], Domain) =
        (lhs.iterator, rhs)

}
