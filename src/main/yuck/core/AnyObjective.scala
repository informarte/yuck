package yuck.core

/**
  * Helper class for passing the result of tightening.
  *
  * @author Michael Marte
  */
case class TighteningResult(val searchState: SearchState, val maybeTightenedVariable: Option[AnyVariable])

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

    /**
     * Tries to tighten this objective such that search states worse than or equivalent
     * (in cost) to the current one become infeasible.
     *
     * Tightening only applies when the underlying objective variable x is a search
     * variable not constrained implicitly.
     *
     * The first step of tightening is to find the best objective value a that is compatible
     * with the current search state (in the sense that its assignment to x does not impair
     * the solution quality), to assign a to x, and to prune all values worse than a from
     * the domain of x.
     *
     * The second step of tightening is to reduce the domain of x in accordance with the
     * purpose of this method (unless this step would wipe out the domain of x) and includes,
     * as a prerequisite, assigning to x the worst value of the reduced domain.
     *
     * The result of this method is twofold:
     * 1. The search state resulting from the first step.
     *    (This may be the search state of the given space.)
     * 2. Some(x) if the domain of x was reduced in the process, and None otherwise.
     */
    final def tighten(space: Space): TighteningResult = tighten(space, this)

    def tighten(space: Space, topLevelObjective: AnyObjective): TighteningResult
}
