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
    override def toString = roundLogs.iterator.map(_.toString).mkString("\n")
    var costsOfInitialProposal: Costs = null
    var costsOfFinalProposal: Costs = null
    val roundLogs = new mutable.ArrayBuffer[RoundLog]
    var indexOfRoundWithBestProposal: Int = 0
    override def isGoodEnough = super.isGoodEnough
    override def isOptimal = super.isOptimal
    def runtimeInSeconds: Double = roundLogs.iterator.map(_.runtimeInSeconds).sum
    def movesPerSecond: Double = roundLogs.iterator.map(_.movesPerSecond).sum / roundLogs.size
    def consultationsPerSecond: Double = roundLogs.iterator.map(_.consultationsPerSecond).sum / roundLogs.size
    def consultationsPerMove: Double = roundLogs.iterator.map(_.consultationsPerMove).sum / roundLogs.size
    def commitmentsPerSecond: Double = roundLogs.iterator.map(_.commitmentsPerSecond).sum / roundLogs.size
    def commitmentsPerMove: Double = roundLogs.iterator.map(_.commitmentsPerMove).sum / roundLogs.size
}
