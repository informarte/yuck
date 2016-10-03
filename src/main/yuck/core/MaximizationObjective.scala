package yuck.core

import scala.math._

/**
 * Objective for maximizing the value of a variable.
 *
 * @author Michael Marte
 */
final class MaximizationObjective
    [Value <: NumericalValue[Value]]
    (x: Variable[Value], override val targetCosts: Value)
    extends AnyObjective
{
    override def toString =
        "max %s".format(x)
    override def costs(searchState: SearchState) =
        searchState.value(x)
    override def isSolution(costs: Costs) =
        costs.asInstanceOf[Value] >= targetCosts
    override def isGoodEnough(costs: Costs) =
        isSolution(costs)
    override def compareCosts(lhs: Costs, rhs: Costs) =
        rhs.asInstanceOf[Value].compare(lhs.asInstanceOf[Value])
    private var deltaScale = 0.0
    private var sampleSize = 0.0
    override def assessMove(before: SearchState, after: SearchState) = {
        var delta = (costs(before) - costs(after)).toDouble
        if (delta != 0) {
            val sign = signum(delta)
            delta = abs(delta)
            // scale compression (log(1) = 0, so shift curve to the left)
            delta = log(0.1 + delta)
            // scale normalization (see http://math.stackexchange.com/questions/106700/incremental-averageing)
            sampleSize += 1
            deltaScale += (delta - deltaScale) / sampleSize
            delta /= deltaScale
            delta *= sign
        }
        delta
    }
    override def topLevelGoalVariable =
        x
}
