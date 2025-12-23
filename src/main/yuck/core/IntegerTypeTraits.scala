package yuck.core

import scala.collection.*

object IntegerTypeTraits extends IntegralTypeTraits[IntegerValue, IntegerDomain, IntegerVariable] {
    override val valueClass = classOf[IntegerValue]
    override val domainClass = classOf[IntegerDomain]
    override val variableClass = classOf[IntegerVariable]
    override val domainCapabilities = DomainCapabilities()
    override def normalizedValue(a: IntegerValue) = a
    override val zero = Zero
    override val one = One
    override val minValue = IntegerValue(Long.MinValue)
    override val maxValue = IntegerValue(Long.MaxValue)
    override val valueOrdering = IntegerValueOperations
    override val numericalOperations = IntegerValueOperations
    override val costModel = IntegerValueOrderingCostModel
    override def createDomain(values: Set[IntegerValue]) = IntegerDomain(values)
    override def createDomain(lb: IntegerValue, ub: IntegerValue) = IntegerRange(lb, ub)
    override val emptyDomain = EmptyIntegerRange
    override val completeDomain = CompleteIntegerRange
    override val nonNegativeDomain = NonNegativeIntegerRange
    override val domainOrdering = IntegerDomainOrdering
    override val domainPruner = IntegerDomainPruner
    override def createVariable(space: Space, name: String, domain: IntegerDomain) =
        new IntegerVariable(space.nextVariableId(), name, safeDowncast(domain))
    override def createChannel(space: Space) =
        new IntegerVariable(space.nextVariableId(), "", completeDomain)
    override def safeDowncast(a: AnyValue) = a.asInstanceOf[IntegerValue]
    override def safeDowncast(x: AnyDomain) = x.asInstanceOf[IntegerDomain]
    override def safeDowncast(x: AnyVariable) = x.asInstanceOf[IntegerVariable]
}
