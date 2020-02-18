package yuck.core

/**
 * Objective for minimizing the value of a variable.
 *
 * @author Michael Marte
 */
final class MinimizationObjective
    [Value <: NumericalValue[Value]]
    (override val x: NumericalVariable[Value],
     maybeTargetCosts: Option[Value],
     override protected val maybeY: Option[NumericalVariable[Value]])
    (implicit valueTraits: NumericalValueTraits[Value])
    extends NumericalObjective[Value]
{
    override def toString =
        "min %s".format(x)
    override def targetCosts: Value = {
        val dx = x.domain
        if (dx.hasLb && maybeTargetCosts.isDefined) valueTraits.valueOrdering.max(dx.lb, maybeTargetCosts.get)
        else if (dx.hasLb) dx.lb
        else if (maybeTargetCosts.isDefined) maybeTargetCosts.get
        else valueTraits.minValue
    }
    override def isOptimal(costs: Costs) = {
        val dx = x.domain
        dx.hasLb && costs.asInstanceOf[Value] <= dx.lb
    }
    override def compareCosts(lhs: Costs, rhs: Costs) =
        lhs.asInstanceOf[Value].compare(rhs.asInstanceOf[Value])
    override protected def computeDelta(before: SearchState, after: SearchState) =
        costs(after).toDouble - costs(before).toDouble
    override val optimizationMode = OptimizationMode.Min
}
