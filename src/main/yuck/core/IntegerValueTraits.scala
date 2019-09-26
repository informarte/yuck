package yuck.core

import scala.collection._

/**
 * Provides traits of integer values.
 *
 * @author Michael Marte
 */
final object IntegerValueTraits extends IntegralValueTraits[IntegerValue] {
    override val valueType = classOf[IntegerValue]
    override val zero = Zero
    override val one = One
    override val valueOrdering = IntegerValueOperations
    override val numericalOperations = IntegerValueOperations
    override val orderingCostModel = IntegerValueOrderingCostModel
    override def createDomain(values: Set[IntegerValue]): IntegerDomain = IntegerDomain.createDomain(values)
    override def createDomain(lb: IntegerValue, ub: IntegerValue) = IntegerDomain.createRange(lb, ub)
    override val emptyDomain: IntegerDomain = EmptyIntegerRange
    override val completeDomain: IntegerDomain = CompleteIntegerRange
    override val nonNegativeDomain: IntegerDomain = NonNegativeIntegerRange
    override val domainOrdering = IntegerDomainOrdering
    override val domainPruner = IntegerDomainPruner
    override def createVariable(space: Space, name: String, domain: Domain[IntegerValue]): IntegerVariable =
        new IntegerVariable(space.nextVariableId, name, safeDowncast(domain))
    override def createChannel(space: Space): IntegerVariable =
        new IntegerVariable(space.nextVariableId, "", completeDomain)
    override def safeDowncast(a: AnyValue): IntegerValue = a.asInstanceOf[IntegerValue]
    override def safeDowncast(x: AnyDomain): IntegerDomain = x.asInstanceOf[IntegerDomain]
    override def safeDowncast(x: AnyVariable): IntegerVariable = x.asInstanceOf[IntegerVariable]
}
