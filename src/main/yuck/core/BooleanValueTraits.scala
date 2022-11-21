package yuck.core

import scala.collection.*

/**
 * Provides traits of Boolean values.
 *
 * @author Michael Marte
 */
object BooleanValueTraits extends OrderedValueTraits[BooleanValue] {
    override val valueType = classOf[BooleanValue]
    override def normalizedValue(a: BooleanValue) = if (a.truthValue) then True else False
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
    override def createVariable(space: Space, name: String, domain: Domain[BooleanValue]): BooleanVariable =
        new BooleanVariable(space.nextVariableId(), name, safeDowncast(domain))
    override def createChannel(space: Space): BooleanVariable =
        new BooleanVariable(space.nextVariableId(), "", CompleteBooleanDomain)
    override def safeDowncast(a: AnyValue): BooleanValue = a.asInstanceOf[BooleanValue]
    override def safeDowncast(x: AnyDomain): BooleanDomain = x.asInstanceOf[BooleanDomain]
    override def safeDowncast(x: AnyVariable): BooleanVariable = x.asInstanceOf[BooleanVariable]
}
