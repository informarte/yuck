package yuck.annealing

import scala.collection.*

import yuck.core.*
import yuck.util.DescriptiveStatistics.*

/**
 * @author Michael Marte
 *
 */
final class AnnealingResult(
    override val maybeUserData: Option[Object],
    override val solverName: String,
    override val objective: AnyObjective,
    override val bestProposal: SearchState,
    override val numberOfMoves: Long,
    override val runtimeInMillis: Long,
    override val numberOfConsultations: Long,
    override val numberOfCommitments: Long,
    override val numberOfPerturbations: Int,
    val roundLogs: IndexedSeq[RoundLog])
    extends LocalSearchResult
