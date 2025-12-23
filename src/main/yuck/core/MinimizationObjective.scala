package yuck.core

/**
 * Objective for minimizing the value of a variable.
 */
final class MinimizationObjective
    [A <: NumericalValue[A], D <: NumericalDomain[A, D], X <: NumericalVariable[A, D, X]]
    (override val x: X,
     maybeTargetCosts: Option[A],
     override val maybeY: Option[X])
    (using typeTraits: NumericalTypeTraits[A, D, X])
    extends NumericalObjective[A, D, X]
{
    override def toString =
        "minimize %s".format(x)
    override def targetCosts: A = {
        val dx = x.domain
        if dx.hasLb && maybeTargetCosts.isDefined
        then typeTraits.valueOrdering.max(dx.lb, maybeTargetCosts.get)
        else if dx.hasLb
        then dx.lb
        else if maybeTargetCosts.isDefined
        then maybeTargetCosts.get
        else typeTraits.minValue
    }
    override def isOptimal(costs: Costs) = {
        val dx = x.domain
        dx.hasLb && costs.asInstanceOf[A] <= dx.lb
    }
    override def compareCosts(lhs: Costs, rhs: Costs) =
        lhs.asInstanceOf[A].compare(rhs.asInstanceOf[A])
    override protected def computeDelta(before: SearchState, after: SearchState) =
        costs(after).toDouble - costs(before).toDouble
    override val optimizationMode = OptimizationMode.Min
}
