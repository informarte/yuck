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
    val roundLogs: IndexedSeq[RoundLog])
    extends Result
{
    override def toString = roundLogs.iterator.map(_.toString).mkString("\n")
    def runtimeInSeconds: Double = roundLogs.map(_.runtimeInSeconds).median
    def movesPerSecond: Double = roundLogs.map(_.movesPerSecond).median
    def consultationsPerSecond: Double = roundLogs.map(_.consultationsPerSecond).median
    def consultationsPerMove: Double = roundLogs.map(_.consultationsPerMove).median
    def commitmentsPerSecond: Double = roundLogs.map(_.commitmentsPerSecond).median
    def commitmentsPerMove: Double = roundLogs.map(_.commitmentsPerMove).median
}
