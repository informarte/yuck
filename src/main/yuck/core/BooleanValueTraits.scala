package yuck.core

/**
 * Provides traits of Boolean values.
 *
 * @author Michael Marte
 */
final object BooleanValueTraits extends NumericalValueTraits[BooleanValue] {
    override val valueType = classOf[BooleanValue]
    override val orderingCostModel = BooleanOrderingCostModel
    override def createDomain(values: Set[BooleanValue]) = {
        require(values.isEmpty || values.toIterator.map(_.violation).max < 2)
        BooleanDecisionDomain.createDomain(values.contains(False), values.contains(True))
    }
    override def createDomain(lb: BooleanValue, ub: BooleanValue) =
        BooleanDecisionDomain.createDomain(lb, ub)
    override val zero = True
    override val one = False
    override val emptyDomain = EmptyBooleanDomain
    override val completeDomain = CompleteBooleanDomain
    override val nonNegativeDomain = completeDomain
}
