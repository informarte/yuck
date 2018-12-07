package yuck.core

import scala.collection._

import yuck.util.arm.Sigint

/**
 * Provides the constraint interface for local search.
 *
 * Regarding the input variables, there are two things to keep in mind
 * when implementing a constraint:
 *
 *  - There may be channel variables among them with infinite domains.
 *  - There may be duplicate variables.
 *
 * Moreover, the domains of both input and output variables may get pruned between
 * construction and initialization.
 *
 * @author Michael Marte
 */
abstract class Constraint
    (val id: Id[Constraint], val goal: Goal)
    extends Ordered[Constraint]
{

    @inline final override def hashCode = id.hashCode
    @inline final override def compare(that: Constraint) = this.id.compare(that.id)

    /** Returns the input variables. */
    def inVariables: TraversableOnce[AnyVariable]

    /** Returns the output variables. */
    def outVariables: TraversableOnce[AnyVariable]

    /**
     * Propgates the constraint by pruning the domains of its variables.
     *
     * Returns true iff some pruning happened.
     *
     * Does not need to compute a fixed point.
     *
     * Intended for use before local search.
     *
     * The default implementation does nothing and returns false.
     */
    def propagate: Boolean = false

    /**
     * Initializes the constraint's internal state according to the given search state
     * and returns values for all output variables by means of effects.
     */
    def initialize(now: SearchState): TraversableOnce[AnyEffect]

    /**
     * Assesses the impact of the given move on the constraint's output variables.
     * @param before is the search state before the move and the one the constraint's internal
     * state is in sync with.
     * @param after is the search state obtained by applying the move to before.
     * @param move is the move to be assessed and involves only input variables of the constraint
     * that are affected by the move.
     */
    def consult(before: SearchState, after: SearchState, move: Move): TraversableOnce[AnyEffect]

    /**
     * Performs the given move by adapting the constraint's internal state accordingly.
     *
     * Please see consult for the parameters' meaning and the expected return value.
     *
     * For use with stateless constraints, the default implementation forwards to consult.
     *
     * A call to commit will only happen after a call to consult.
     */
    def commit(before: SearchState, after: SearchState, move: Move): TraversableOnce[AnyEffect] =
        consult(before, after, move)

    /**
     * Returns true if this constraint is a candidate for implicit solving
     * with respect to the given space.
     *
     * The default implementation returns false.
     */
    def isCandidateForImplicitSolving(space: Space): Boolean = false

    /**
     * Prepares the grounds for solving this constraint implicitly.
     *
     *  - Assigns values to the constraint's search variables such that the
     *    assignment satisfies the constraint.
     *  - Assigns zero to the constraint's cost variable.
     *  - Creates and returns a neighbourhood that maintains feasibility.
     *
     * The implementation should not assume that this constraint has been posted.
     *
     * The default implementation does nothing and returns None.
     */
    def prepareForImplicitSolving(
        space: Space,
        randomGenerator: RandomGenerator,
        moveSizeDistribution: Distribution,
        hotSpotDistributionFactory: Seq[AnyVariable] => Option[Distribution],
        maybeFairVariableChoiceRate: Option[Probability],
        sigint: Sigint):
        Option[Neighbourhood] =
        None

}
