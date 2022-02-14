package yuck.core

import scala.collection.*

/**
 * Provides traits of Boolean values.
 *
 * @author Michael Marte
 */
object BooleanValueTraits extends OrderedValueTraits[BooleanValue] {
    override val valueType = classOf[BooleanValue]
    override val valueOrdering = BooleanValueOrdering
    override val orderingCostModel = BooleanValueOrderingCostModel
    override def createDomain(values: Set[BooleanValue]) = {
        require(! values.exists(_.violation >= 2))
        BooleanDecisionDomain(values.contains(False), values.contains(True))
    }
    override def createDomain(lb: BooleanValue, ub: BooleanValue) =
        BooleanDecisionDomain(lb, ub)
    override val emptyDomain = EmptyBooleanDomain
    override val completeDomain = CompleteBooleanDomain
    override val domainOrdering = BooleanDomainOrdering
    override val domainPruner = BooleanDomainPruner
    override def createVariable(space: Space, name: String, domain: Domain[BooleanValue]): BooleanVariable =
        new BooleanVariable(space.nextVariableId, name, safeDowncast(domain))
    override def createChannel(space: Space): BooleanVariable =
        // Using CompleteBooleanDecisionDomain instead of CompleteBooleanDomain speeds up propagation.
        new BooleanVariable(space.nextVariableId, "", CompleteBooleanDecisionDomain)
    override def safeDowncast(a: AnyValue): BooleanValue = a.asInstanceOf[BooleanValue]
    override def safeDowncast(x: AnyDomain): BooleanDomain = x.asInstanceOf[BooleanDomain]
    override def safeDowncast(x: AnyVariable): BooleanVariable = x.asInstanceOf[BooleanVariable]
}
