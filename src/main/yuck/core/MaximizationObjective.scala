package yuck.core

/**
 * Objective for maximizing the value of a variable.
 */
final class MaximizationObjective
    [A <: NumericalValue[A], D <: NumericalDomain[A, D], X <: NumericalVariable[A, D, X]]
    (override val x: X,
     maybeTargetCosts: Option[A],
     override val maybeY: Option[X])
    (using typeTraits: NumericalTypeTraits[A, D, X])
    extends NumericalObjective[A, D, X]
{
    override def toString =
        "maximize %s".format(x)
    override def targetCosts: A = {
        val dx = x.domain
        if dx.hasUb && maybeTargetCosts.isDefined
        then typeTraits.valueOrdering.min(dx.ub, maybeTargetCosts.get)
        else if dx.hasUb
        then dx.ub
        else if maybeTargetCosts.isDefined
        then maybeTargetCosts.get
        else typeTraits.maxValue
    }
    override def isOptimal(costs: Costs) = {
        val dx = x.domain
        dx.hasUb && costs.asInstanceOf[A] >= dx.ub
    }
    override def compareCosts(lhs: Costs, rhs: Costs) =
        rhs.asInstanceOf[A].compare(lhs.asInstanceOf[A])
    override protected def computeDelta(before: SearchState, after: SearchState) =
        costs(before).toDouble - costs(after).toDouble
    override val optimizationMode = OptimizationMode.Max
}
