package yuck.core

import scala.collection.*

/**
 * Provides traits of integer-set values.
 *
 * @author Michael Marte
 */
implicit object IntegerSetValueTraits extends OrderedValueTraits[IntegerSetValue] {
    override val valueType = classOf[IntegerSetValue]
    override val valueOrdering = IntegerSetValueOrdering
    override val orderingCostModel = IntegerSetValueOrderingCostModel
    override def createDomain(values: Set[IntegerSetValue]): IntegerSetDomain =
        if (values.isEmpty) EmptyIntegerSetDomain else ???
    override def createDomain(lb: IntegerSetValue, ub: IntegerSetValue): IntegerSetDomain =
        if (ub < lb) EmptyIntegerSetDomain
        else if (lb == ub) new SingletonIntegerSetDomain(lb.set)
        else if (lb.set.isEmpty) new IntegerPowersetDomain(ub.set)
        else ???
    override val emptyDomain: IntegerSetDomain = EmptyIntegerSetDomain
    override val completeDomain: IntegerSetDomain = CompleteIntegerSetDomain
    override val domainPruner = IntegerSetDomainPruner
    override val domainOrdering = IntegerSetDomainOrdering
    override def createVariable(space: Space, name: String, domain: Domain[IntegerSetValue]): IntegerSetVariable =
        new IntegerSetVariable(space.nextVariableId(), name, safeDowncast(domain))
    override def createChannel(space: Space): IntegerSetVariable =
        new IntegerSetVariable(space.nextVariableId(), "", completeDomain)
    override def safeDowncast(a: AnyValue): IntegerSetValue = a.asInstanceOf[IntegerSetValue]
    override def safeDowncast(x: AnyDomain): IntegerSetDomain = x.asInstanceOf[IntegerSetDomain]
    override def safeDowncast(x: AnyVariable): IntegerSetVariable = x.asInstanceOf[IntegerSetVariable]
}
