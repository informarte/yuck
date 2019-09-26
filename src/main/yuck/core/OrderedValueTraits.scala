package yuck.core

import scala.collection._

/**
 * Provides properties of ordered values.
 *
 * @author Michael Marte
 */
abstract class OrderedValueTraits[Value <: OrderedValue[Value]] extends ValueTraits[Value] {

    override def createDomain(values: Set[Value]): OrderedDomain[Value]
    override val emptyDomain: OrderedDomain[Value]
    override val completeDomain: OrderedDomain[Value]
    override val domainPruner: OrderedDomainPruner[Value]
    override def createVariable(space: Space, name: String, domain: Domain[Value]): OrderedVariable[Value]
    override def createChannel(space: Space): OrderedVariable[Value]
    override def safeDowncast(d: AnyDomain): OrderedDomain[Value]
    override def safeDowncast(x: AnyVariable): OrderedVariable[Value]

    /** A total ordering on Value. */
    val valueOrdering: Ordering[Value]

    /** A total ordering on OrderedDomain[Value]. */
    val domainOrdering: Ordering[OrderedDomain[Value]]

    /** The cost model for ordering operations over Value. */
    val orderingCostModel: OrderingCostModel[Value]

    /** Creates a domain from the given bounds. */
    def createDomain(lb: Value, ub: Value): OrderedDomain[Value]

}
