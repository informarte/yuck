package yuck.annealing

import scala.collection._

import yuck.core._

/**
 * @author Michael Marte
 *
 */
final class AnnealingResult(
    solverName: String,
    space: Space,
    objective: AnyObjective,
    maybeUserData: Option[Object])
    extends Result(solverName, space, objective, maybeUserData)
{
    override def toString = roundLogs.map(_.toString).mkString("\n")
    var costsOfInitialProposal: Costs = null
    var costsOfFinalProposal: Costs = null
    val roundLogs = new mutable.ArrayBuffer[RoundLog]
    var indexOfRoundWithBestProposal: Int = 0
    var runtimeInMillis: Long = 0
    def movesPerSecond: Int = roundLogs.toIterator.map(_.movesPerSecond).sum / roundLogs.size
    def consultationsPerSecond: Int = roundLogs.toIterator.map(_.consultationsPerSecond).sum / roundLogs.size
    def consultationsPerMove: Int = roundLogs.toIterator.map(_.consultationsPerMove).sum / roundLogs.size
    def commitmentsPerSecond: Int = roundLogs.toIterator.map(_.commitmentsPerSecond).sum / roundLogs.size
    def commitmentsPerMove: Int = roundLogs.toIterator.map(_.commitmentsPerMove).sum / roundLogs.size
}
