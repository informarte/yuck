package yuck.core

/**
 * Provides methods for pruning integer-set domains.
 *
 * @author Michael Marte
 */
final object IntegerSetDomainPruner extends OrderedDomainPruner[IntegerSetValue] {

    type DomainImpl = IntegerSetDomain

    override def eq
        [Domain >: DomainImpl <: yuck.core.Domain[IntegerSetValue]]
        (lhs: Domain, rhs: Domain):
        (Domain, Domain) =
    {
        val intersection = lhs.asInstanceOf[DomainImpl].intersect(rhs)
        (intersection, intersection)
    }

}
