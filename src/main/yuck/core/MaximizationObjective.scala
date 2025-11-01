package yuck.core

/**
 * Objective for maximizing the value of a variable.
 *
 * @author Michael Marte
 */
final class MaximizationObjective
    [V <: NumericalValue[V]]
    (override val x: NumericalVariable[V],
     maybeTargetCosts: Option[V],
     override val maybeY: Option[NumericalVariable[V]])
    (using valueTraits: NumericalValueTraits[V])
    extends NumericalObjective[V]
{
    override def toString =
        "maximize %s".format(x)
    override def targetCosts: V = {
        val dx = x.domain
        if dx.hasUb && maybeTargetCosts.isDefined
        then valueTraits.valueOrdering.min(dx.ub, maybeTargetCosts.get)
        else if dx.hasUb
        then dx.ub
        else if maybeTargetCosts.isDefined
        then maybeTargetCosts.get
        else valueTraits.maxValue
    }
    override def isOptimal(costs: Costs) = {
        val dx = x.domain
        dx.hasUb && costs.asInstanceOf[V] >= dx.ub
    }
    override def compareCosts(lhs: Costs, rhs: Costs) =
        rhs.asInstanceOf[V].compare(lhs.asInstanceOf[V])
    override protected def computeDelta(before: SearchState, after: SearchState) =
        costs(before).toDouble - costs(after).toDouble
    override val optimizationMode = OptimizationMode.Max
}
