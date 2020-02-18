package yuck.core

/**
 * Represents an optimization goal for use in local search.
 *
 * @author Michael Marte
 */
abstract class AnyObjective {

    /** Returns the primitive objectives this objective is composed of. */
    def primitiveObjectives: Seq[PrimitiveObjective]

    /** Returns the variables subject to optimization. */
    def objectiveVariables: Seq[AnyVariable]

    /** Returns the quality to achieve. */
    def targetCosts: Costs

    /** Measures the quality of the given search state. */
    def costs(searchState: SearchState): Costs

    /** Decides whether a search state (given in terms of its quality) is a solution. */
    def isSolution(costs: Costs): Boolean

    /** Decides whether the given search state is a solution. */
    def isSolution(searchState: SearchState): Boolean = isSolution(costs(searchState))

    /** Decides whether a search state (given in terms of its quality) satisfies the objective. */
    def isGoodEnough(costs: Costs): Boolean = ! isHigherThan(costs, targetCosts)

    /** Decides whether the given search state satisfies the objective. */
    def isGoodEnough(searchState: SearchState): Boolean = isGoodEnough(costs(searchState))

    /** Decides whether a search state (given in terms of its quality) is an optimal solution. */
    def isOptimal(costs: Costs): Boolean

    /**
     * Returns true iff optimality has been proven.
     *
     * (A negative results hence means that the status is unknown.)
     */
    def isOptimal(searchState: SearchState): Boolean = isOptimal(costs(searchState))

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
     * Addresses a problem with objective variables that are search variables:
     * Finds the best objective value a that is compatible with the current search state
     * (in the sense that x = a does not violate more important objectives) and assigns a to x.
     */
    final def findActualObjectiveValue(space: Space): Unit = findActualObjectiveValue(space, this)

    private[core] def findActualObjectiveValue(space: Space, rootObjective: AnyObjective): Unit

    /**
     * Tries to tighten this objective (by reducing variable domains) such that search states
     * worse than or equivalent to the current one become infeasible.
     *
     * Returns the set of variables the domains of which were reduced in the process.
     */
    def tighten(space: Space): Set[AnyVariable]

}
