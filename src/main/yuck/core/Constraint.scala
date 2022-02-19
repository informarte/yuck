package yuck.core

import scala.collection.*

import yuck.util.arm.Sigint
import yuck.util.logging.LazyLogger

/**
 * Provides additional configuration to Constraint#createNeighbourhood.
 *
 * @author Michael Marte
 */
final case class ExtraNeighbourhoodFactoryConfiguration(
    createHotSpotDistribution: Seq[AnyVariable] => Option[Distribution] = _ => None,
    maybeFairVariableChoiceRate: Option[Probability] = None,
    checkIncrementalCostUpdate: Boolean = false,
    checkAssignmentsToNonChannelVariables: Boolean = false,
    maxNumberOfGreedyHeuristicRuns: Int = 100)

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
abstract class Constraint(val id: Id[Constraint]) extends Ordered[Constraint] {

    /** The optimization goal this constraint contributes to. */
    val maybeGoal: Option[Goal] = None

    @inline final override def hashCode = id.rawId

    @inline final override def compare(that: Constraint) = this.id.rawId - that.id.rawId
    @inline final def ==(that: Constraint): Boolean = this.id.rawId == that.id.rawId
    @inline final def !=(that: Constraint): Boolean = this.id.rawId != that.id.rawId
    @inline final override def <(that: Constraint): Boolean = this.id.rawId < that.id.rawId
    @inline final override def <=(that: Constraint): Boolean = this.id.rawId <= that.id.rawId
    @inline final override def >(that: Constraint): Boolean = this.id.rawId > that.id.rawId
    @inline final override def >=(that: Constraint): Boolean = this.id.rawId >= that.id.rawId

    /** Returns the input variables. */
    def inVariables: Iterable[AnyVariable]

    /** Returns the output variables. */
    def outVariables: Iterable[AnyVariable]

    /**
     * Propagates the constraint by pruning the domains of its variables.
     *
     * Does not need to compute a fixed point.
     *
     * The default implementation does nothing.
     */
    def propagate(): PropagationEffects = NoPropagationOccurred

    /**
     * Initializes the constraint's internal state according to the given search state
     * and returns values for all output variables by means of effects.
     */
    def initialize(now: SearchState): Iterable[AnyMoveEffect]

    /**
     * Assesses the impact of the given move on the constraint's output variables.
     * @param before is the search state before the move and the one the constraint's internal
     * state is in sync with.
     * @param after is the search state obtained by applying the move to before.
     * @param move is the move to be assessed and involves only input variables of the constraint
     * that are affected by the move.
     */
    def consult(before: SearchState, after: SearchState, move: Move): Iterable[AnyMoveEffect]

    /**
     * Performs the given move by adapting the constraint's internal state accordingly.
     *
     * Please see consult for the parameters' meaning and the expected return value.
     *
     * For use with stateless constraints, the default implementation forwards to consult.
     *
     * A call to commit will only happen after a call to consult.
     */
    def commit(before: SearchState, after: SearchState, move: Move): Iterable[AnyMoveEffect] =
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
     *  - Assigns True to the constraint's cost variable.
     *  - Creates and returns a neighbourhood that maintains feasibility.
     *
     * The implementation can assume that this constraint has been posted
     * and that it is a candidate for implicit solving.
     *
     * The default implementation does nothing and returns None.
     */
    def createNeighbourhood(
        space: Space,
        randomGenerator: RandomGenerator,
        moveSizeDistribution: Distribution,
        logger: LazyLogger,
        sigint: Sigint,
        extraCfg: ExtraNeighbourhoodFactoryConfiguration = ExtraNeighbourhoodFactoryConfiguration()):
        Option[Neighbourhood] =
        None

}
