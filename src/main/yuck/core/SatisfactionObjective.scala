package yuck.core

/**
 * Objective to satisfy a Boolean expression.
 *
 * @author Michael Marte
 */
final class SatisfactionObjective(override val x: BooleanVariable) extends PrimitiveObjective {
    override def toString = "satisfy %s".format(x)
    override def targetCosts = True
    override def costs(searchState: SearchState): BooleanValue = searchState.value(x)
    override def isSolution(costs: Costs): Boolean = costs == True
    override def isGoodEnough(costs: Costs): Boolean = costs == True
    override def isOptimal(costs: Costs): Boolean = costs == True
    override def compareCosts(lhs: Costs, rhs: Costs) =
        lhs.asInstanceOf[BooleanValue].compare(rhs.asInstanceOf[BooleanValue])
    override protected def computeDelta(before: SearchState, after: SearchState) =
        costs(after).violation.toDouble - costs(before).violation.toDouble
    override val optimizationMode = OptimizationMode.Min
}
