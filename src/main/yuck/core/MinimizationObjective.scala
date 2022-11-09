package yuck.core

/**
 * Objective for minimizing the value of a variable.
 *
 * @author Michael Marte
 */
final class MinimizationObjective
    [V <: NumericalValue[V]]
    (override val x: NumericalVariable[V],
     maybeTargetCosts: Option[V],
     override protected val maybeY: Option[NumericalVariable[V]])
    (using valueTraits: NumericalValueTraits[V])
    extends NumericalObjective[V]
{
    override def toString =
        "min %s".format(x)
    override def targetCosts: V = {
        val dx = x.domain
        if (dx.hasLb && maybeTargetCosts.isDefined) valueTraits.valueOrdering.max(dx.lb, maybeTargetCosts.get)
        else if (dx.hasLb) dx.lb
        else if (maybeTargetCosts.isDefined) maybeTargetCosts.get
        else valueTraits.minValue
    }
    override def isOptimal(costs: Costs) = {
        val dx = x.domain
        dx.hasLb && costs.asInstanceOf[V] <= dx.lb
    }
    override def compareCosts(lhs: Costs, rhs: Costs) =
        lhs.asInstanceOf[V].compare(rhs.asInstanceOf[V])
    override protected def computeDelta(before: SearchState, after: SearchState) =
        costs(after).toDouble - costs(before).toDouble
    override val optimizationMode = OptimizationMode.Min
}
