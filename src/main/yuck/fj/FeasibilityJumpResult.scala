package yuck.fj

import yuck.core.*

/**
 * @author Michael Marte
 *
 */
final class FeasibilityJumpResult(
    override val maybeUserData: Option[Object],
    override val solverName: String,
    override val objective: AnyObjective,
    override val bestProposal: SearchState,
    override val numberOfMoves: Long,
    override val runtimeInMillis: Long,
    override val numberOfConsultations: Long,
    override val numberOfCommitments: Long,
    override val numberOfPerturbations: Int)
    extends LocalSearchResult
