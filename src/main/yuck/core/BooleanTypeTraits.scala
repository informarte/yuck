package yuck.core

import scala.collection.*

object BooleanTypeTraits extends OrderedTypeTraits[BooleanValue, BooleanDomain, BooleanVariable] {
    override val valueClass = classOf[BooleanValue]
    override val domainClass = classOf[BooleanDomain]
    override val variableClass = classOf[BooleanVariable]
    override val domainCapabilities = DomainCapabilities()
    override def normalizedValue(a: BooleanValue) = if a.truthValue then True else False
    override val valueOrdering = BooleanValueOrdering
    override val costModel = BooleanValueOrderingCostModel
    override def createDomain(values: Set[BooleanValue]) = {
        require(! values.exists(_.violation >= 2))
        BooleanDomain(values.contains(False), values.contains(True))
    }
    override def createDomain(lb: BooleanValue, ub: BooleanValue) =
        BooleanDomain(lb, ub)
    override val emptyDomain = EmptyBooleanDomain
    override val completeDomain = CompleteBooleanDomain
    override val domainOrdering = BooleanDomainOrdering
    override val domainPruner = BooleanDomainPruner
    override def createVariable(space: Space, name: String, domain: BooleanDomain) =
        new BooleanVariable(space.nextVariableId(), name, domain)
    override def createChannel(space: Space) =
        new BooleanVariable(space.nextVariableId(), "", CompleteBooleanDomain)
    override def safeDowncast(a: AnyValue) = a.asInstanceOf[BooleanValue]
    override def safeDowncast(x: AnyDomain) = x.asInstanceOf[BooleanDomain]
    override def safeDowncast(x: AnyVariable) = x.asInstanceOf[BooleanVariable]
}
