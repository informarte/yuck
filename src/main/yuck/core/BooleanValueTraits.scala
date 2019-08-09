package yuck.core

/**
 * Provides traits of Boolean values.
 *
 * @author Michael Marte
 */
final object BooleanValueTraits extends NumericalValueTraits[BooleanValue] {
    override val valueType = classOf[BooleanValue]
    override val orderingCostModel = BooleanOrderingCostModel
    override val zero = True
    override val one = False
    override def createDomain(values: Set[BooleanValue]) = {
        require(!values.exists(_.violation >= 2))
        BooleanDecisionDomain.createDomain(values.contains(False), values.contains(True))
    }
    override def createDomain(lb: BooleanValue, ub: BooleanValue) =
        BooleanDecisionDomain.createDomain(lb, ub)
    override val emptyDomain = EmptyBooleanDomain
    override val completeDomain = CompleteBooleanDomain
    override val nonNegativeDomain = completeDomain
    override val domainPruner = BooleanDomainPruner
    override def createVariable(space: Space, name: String, domain: Domain[BooleanValue]): BooleanVariable =
        new BooleanVariable(space.nextVariableId, name, safeDowncast(domain))
    override def createChannel(space: Space): BooleanVariable =
        new BooleanVariable(space.nextVariableId, "", completeDomain)
    override def safeDowncast(a: AnyValue): BooleanValue = a.asInstanceOf[BooleanValue]
    override def safeDowncast(x: AnyDomain): BooleanDomain = x.asInstanceOf[BooleanDomain]
    override def safeDowncast(x: AnyVariable): BooleanVariable = x.asInstanceOf[BooleanVariable]
}
