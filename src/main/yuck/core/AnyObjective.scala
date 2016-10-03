package yuck.core

/**
 * Represents an optimization goal for use in local search.
 *
 * @author Michael Marte
 */
abstract class AnyObjective {

    /** Returns the quality to achieve. */
    val targetCosts: Costs

    /** Measures the quality of the given search state. */
    def costs(searchState: SearchState): Costs

    /** Decides whether the search state given in terms of its quality is a solution. */
    def isSolution(costs: Costs): Boolean

    /** Decides whether the search state given in terms of its quality satisfies the objective. */
    def isGoodEnough(costs: Costs): Boolean = ! isHigherThan(costs, targetCosts)

    /**
     * Given a move in terms of the search states that precede and succeed it,
     * this method assesses the impact of the move by computing the delta in quality.
     *
     *  - If the move does not impact the quality, the result is 0.
     *  - If the move improves the quality, the result is negative.
     *  - If the move impairs the quality, the result is positive.
     *
     * The result is a floating-point number and hence allows the implementation
     * to scale the values, for example.
     */
    def assessMove(before: SearchState, after: SearchState): Double

    /**
     * Compares the given costs.
     *
     *  - If lhs equals rhs, the result is 0.
     *  - If lhs is smaller than rhs, the result is negative.
     *  - Otherwise, the result is positive.
     */
    def compareCosts(lhs: Costs, rhs: Costs): Int

    /** Compares the given costs and decides whether lhs is lower than rhs. */
    @inline final def isLowerThan(lhs: Costs, rhs: Costs): Boolean = compareCosts(lhs, rhs) < 0

    /** Compares the given costs and decides whether lhs is higher than rhs. */
    @inline final def isHigherThan(lhs: Costs, rhs: Costs): Boolean = compareCosts(lhs, rhs) > 0

    /** Returns the top-level goal in terms of the variable that is subject to optimization. */
    def topLevelGoalVariable: AnyVariable

}
