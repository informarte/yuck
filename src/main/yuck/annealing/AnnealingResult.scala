package yuck.annealing

import scala.collection.*

import yuck.core.*
import yuck.util.DescriptiveStatistics.*

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
    def runtimeInSeconds: Double = roundLogs.map(_.runtimeInSeconds).median
    def movesPerSecond: Double = roundLogs.map(_.movesPerSecond).median
    def consultationsPerSecond: Double = roundLogs.map(_.consultationsPerSecond).median
    def consultationsPerMove: Double = roundLogs.map(_.consultationsPerMove).median
    def commitmentsPerSecond: Double = roundLogs.map(_.commitmentsPerSecond).median
    def commitmentsPerMove: Double = roundLogs.map(_.commitmentsPerMove).median
}
