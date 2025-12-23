package yuck.core

import scala.collection.*

object IntegerSetTypeTraits extends OrderedTypeTraits[IntegerSetValue, IntegerSetDomain, IntegerSetVariable] {
    override val valueClass = classOf[IntegerSetValue]
    override val domainClass = classOf[IntegerSetDomain]
    override val variableClass = classOf[IntegerSetVariable]
    override val domainCapabilities = DomainCapabilities(
        createDomain = false,
        diff = false,
        randomSubdomain = false,
        size = false,
        union = false
    )
    override def normalizedValue(a: IntegerSetValue) = a
    override val valueOrdering = IntegerSetValueOrdering
    override val costModel = IntegerSetValueOrderingCostModel
    override def createDomain(values: Set[IntegerSetValue]) =
        if values.isEmpty then EmptyIntegerSetDomain else ???
    override def createDomain(lb: IntegerSetValue, ub: IntegerSetValue) =
        if ub < lb
        then EmptyIntegerSetDomain
        else if lb == ub
        then new SingletonIntegerSetDomain(lb.set)
        else if lb.set.isEmpty
        then new IntegerPowerSetDomain(ub.set)
        else ???
    override val emptyDomain = EmptyIntegerSetDomain
    override val completeDomain = CompleteIntegerSetDomain
    override val domainPruner = IntegerSetDomainPruner
    override val domainOrdering = IntegerSetDomainOrdering
    override def createVariable(space: Space, name: String, domain: IntegerSetDomain) =
        new IntegerSetVariable(space.nextVariableId(), name, safeDowncast(domain))
    override def createChannel(space: Space) =
        new IntegerSetVariable(space.nextVariableId(), "", completeDomain)
    override def safeDowncast(a: AnyValue) = a.asInstanceOf[IntegerSetValue]
    override def safeDowncast(x: AnyDomain) = x.asInstanceOf[IntegerSetDomain]
    override def safeDowncast(x: AnyVariable) = x.asInstanceOf[IntegerSetVariable]
}
