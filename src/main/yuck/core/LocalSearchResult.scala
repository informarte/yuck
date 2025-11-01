package yuck.core

/**
 * @author Michael Marte
 *
 */
abstract class LocalSearchResult extends Result {
    val numberOfMoves: Long
    val numberOfConsultations: Long
    val numberOfCommitments: Long
    val numberOfPerturbations: Int
    override def searchWasPerformed = numberOfMoves > 0
    def movesPerSecond: Double = numberOfMoves / runtimeInSeconds
    def consultationsPerSecond: Double = numberOfConsultations / runtimeInSeconds
    def consultationsPerMove: Double = numberOfConsultations.toDouble / numberOfMoves
    def commitmentsPerSecond: Double = numberOfCommitments / runtimeInSeconds
    def commitmentsPerMove: Double = numberOfCommitments.toDouble / numberOfMoves
}
