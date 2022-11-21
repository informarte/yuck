package yuck.core

import scala.collection.*

/**
 * Provides properties of ordered values.
 *
 * @author Michael Marte
 */
abstract class OrderedValueTraits[V <: OrderedValue[V]] extends ValueTraits[V] {

    override def createDomain(values: Set[V]): OrderedDomain[V]
    override val emptyDomain: OrderedDomain[V]
    override val completeDomain: OrderedDomain[V]
    override val costModel: OrderingCostModel[V]
    override val domainPruner: OrderedDomainPruner[V]
    override def createVariable(space: Space, name: String, domain: Domain[V]): OrderedVariable[V]
    override def createChannel(space: Space): OrderedVariable[V]
    override def safeDowncast(d: AnyDomain): OrderedDomain[V]
    override def safeDowncast(x: AnyVariable): OrderedVariable[V]

    /** A total ordering on V. */
    val valueOrdering: Ordering[V]

    /** A total ordering on OrderedDomain[V]. */
    val domainOrdering: Ordering[OrderedDomain[V]]

    /** Creates a domain from the given bounds. */
    def createDomain(lb: V, ub: V): OrderedDomain[V]

}
