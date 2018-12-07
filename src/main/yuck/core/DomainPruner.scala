package yuck.core

/**
 * Domain pruner interface for use by generic constraints.
 *
 * @author Michael Marte
 */
abstract class DomainPruner[Value <: AnyValue] {

    type DomainImpl <: Domain[Value]

    def eq
        [Domain >: DomainImpl <: yuck.core.Domain[Value]]
        (lhs: Domain, rhs: Domain): (Domain, Domain) =
        (lhs, rhs)

    def ne
        [Domain >: DomainImpl <: yuck.core.Domain[Value]]
        (lhs: Domain, rhs: Domain):
        (Domain, Domain) =
        (lhs, rhs)

}
