package yuck.annealing

import scala.collection.IndexedSeq

import yuck.core.{AnyObjective, LocalSearchResult, SearchState}

final class AnnealingResult(
    override val maybeUserData: Option[Object],
    override val solverName: String,
    override val objective: AnyObjective,
    override val bestProposal: SearchState,
    override val numberOfMoves: Long,
    override val runtimeInMillis: Long,
    override val numberOfPerturbations: Int,
    val roundLogs: IndexedSeq[RoundLog])
    extends LocalSearchResult
{
    override val numberOfConsultations = roundLogs.view.map(_.numberOfConsultations).sum
    override val numberOfCommitments = roundLogs.view.map(_.numberOfCommitments).sum
}
