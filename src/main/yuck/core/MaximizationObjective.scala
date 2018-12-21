package yuck.core

import scala.math._

/**
 * Objective for maximizing the value of a variable.
 *
 * @author Michael Marte
 */
final class MaximizationObjective
    [Value <: NumericalValue[Value]]
    (x: NumericalVariable[Value],
     override val targetCosts: Value,
     maybeTighteningStep: Option[Value])
    (implicit valueTraits: NumericalValueTraits[Value])
    extends NumericalObjective[Value](x)
{
    require(maybeTighteningStep.isEmpty || maybeTighteningStep.get > valueTraits.zero)
    override def toString =
        "max %s".format(x)
    override def isSolution(costs: Costs) =
        costs.asInstanceOf[Value] >= targetCosts
    override def compareCosts(lhs: Costs, rhs: Costs) =
        rhs.asInstanceOf[Value].compare(lhs.asInstanceOf[Value])
    protected override def computeDelta(before: SearchState, after: SearchState) =
        costs(before).toDouble - costs(after).toDouble
    override def tighten(space: Space, rootObjective: AnyObjective) = {
        if (maybeTighteningStep.isDefined) {
            tighten(space, rootObjective, maybeTighteningStep.get)
        } else {
            (space.searchState, None)
        }
    }
}
