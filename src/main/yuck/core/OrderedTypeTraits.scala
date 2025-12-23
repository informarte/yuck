package yuck.core

/**
 * Provides properties of ordered types.
 */
abstract class OrderedTypeTraits
    [A <: OrderedValue[A], D <: OrderedDomain[A, D], X <: Variable[A, D, X]]
    extends TypeTraits[A, D, X]
{

    override val costModel: OrderingCostModel[A]
    override val domainPruner: OrderedDomainPruner[A, D]

    /** The standard total ordering on V. */
    val valueOrdering: Ordering[A]

    /** The standard total ordering on D. */
    val domainOrdering: Ordering[D]

    /** Creates a domain from the given bounds. */
    def createDomain(lb: A, ub: A): D

}
