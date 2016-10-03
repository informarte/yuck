package yuck.annealing

import yuck.core._

/**
 * @author Michael Marte
 *
 */
final class RoundLog(val roundIndex: Int) {
    override def toString =
        "%d;%3.6f;%1.6f;%1.6f;%s;%s;%s;%s;%s;%d;%s;%d;%d;%d;%d;%d;%d;%d".format(
            roundIndex,
            temperature, traditionalAcceptanceRatio, uphillAcceptanceRatio,
            costsOfInitialProposal, costsOfFinalProposal, costsOfBestProposal,
            bestProposalWasImproved, roundWasFutile,
            numberOfMonteCarloAttempts,
            runtimeInSeconds,
            movesPerSecond,
            numberOfConsultations, consultationsPerSecond, consultationsPerMove,
            numberOfCommitments, commitmentsPerSecond, commitmentsPerMove)
    var temperature: Double = 0.0
    var numberOfMonteCarloAttempts: Int = 0
    var numberOfRejectedMoves: Int = 0
    var numberOfAcceptedUphillMoves: Int  = 0
    var costsOfInitialProposal: Costs = null
    var costsOfFinalProposal: Costs = null
    var costsOfBestProposal: Costs = null
    /** The fraction of accepted moves. */
    var traditionalAcceptanceRatio: Double = 0.0
    /** The fraction of accepted uphill moves. */
    var uphillAcceptanceRatio: Double = 0.0
    /** The round was able to improve on the best solution known so far. */
    var bestProposalWasImproved: Boolean = false
    /** The best solution found during the last round was not better than the best
     * solution found in the second to last round. */
    var roundWasFutile: Boolean = false
    /** The runtime in milliseconds. */
    var runtimeInMillis: Long = 0
    /** How often Constraint.consult was called. */
    var numberOfConsultations: Int = 0
    /** How often Constraint.commit was called. */
    var numberOfCommitments: Int = 0
    def runtimeInSeconds: Double = scala.math.max(1l, runtimeInMillis).toDouble / 1000.0
    def movesPerSecond: Int = (numberOfMonteCarloAttempts.toDouble / runtimeInSeconds).toInt
    def consultationsPerSecond: Int = (numberOfConsultations.toDouble / runtimeInSeconds).toInt
    def consultationsPerMove: Int = (numberOfConsultations.toDouble / numberOfMonteCarloAttempts.toDouble).toInt
    def commitmentsPerSecond: Int = (numberOfCommitments.toDouble / runtimeInSeconds).toInt
    def commitmentsPerMove: Int = (numberOfCommitments.toDouble / numberOfMonteCarloAttempts.toDouble).toInt
    def updateAcceptanceRatio {
        val numberOfAcceptedMoves = numberOfMonteCarloAttempts - numberOfRejectedMoves
        traditionalAcceptanceRatio = numberOfAcceptedMoves.toDouble / numberOfMonteCarloAttempts
        val numberOfProposedUphillMoves = numberOfAcceptedUphillMoves + numberOfRejectedMoves
        uphillAcceptanceRatio =
            if (numberOfProposedUphillMoves == 0) 1.0
            else numberOfAcceptedUphillMoves.toDouble / numberOfProposedUphillMoves.toDouble
    }
}
