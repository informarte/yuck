package yuck.core

/**
 * Domain pruner interface for use by generic constraints.
 *
 * @author Michael Marte
 */
abstract class NumericalDomainPruner[Value <: NumericalValue[Value]] extends OrderedDomainPruner[Value] {

    type DomainImpl <: NumericalDomain[Value]

    def linEq
        [Domain >: DomainImpl <: NumericalDomain[Value]]
        (lhs: Iterable[(Value, Domain)], rhs: Domain):
        (TraversableOnce[Domain], Domain) =
        (lhs.iterator.map(_._2), rhs)

    def times
        [Domain >: DomainImpl <: NumericalDomain[Value]]
        (dx: Domain, dy: Domain, dz: Domain):
        (Domain, Domain, Domain) =
        (dx, dy, dz)

}
