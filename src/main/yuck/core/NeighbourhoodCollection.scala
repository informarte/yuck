package yuck.core

import scala.collection.*

import yuck.util.arm.scoped

/**
 * Turns the given collection of neighbourhoods into a neighbourhood.
 *
 * To generate a move, a subset of neighbourhoods is chosen, each one is asked for a move,
 * and the resulting moves are merged into one.
 *
 * Neighbourhood choice can happen in two ways:
 * In fair mode, all neighbourhoods are equally likely to get chosen while
 * in unfair mode the selection probability may be skewed in some way.
 *
 * To facilitate unfair choice, a so-called hot-spot distribution has to be given.
 *
 * With unfair choice enabled, the probability of fair choice comes into play.
 *
 * The implementation of fair neighbourhood choice is based on the neighbourhood sizes:
 * The probability of choosing a neighbourhood is proportional to the number of its
 * search variables.
 *
 * Falls back to fair mode when the given hot-spot distribution has zero volume.
 *
 * @author Michael Marte
 */
final class NeighbourhoodCollection(
    space: Space,
    neighbourhoods: immutable.IndexedSeq[Neighbourhood],
    randomGenerator: RandomGenerator,
    maybeSelectionSizeDistribution: Option[Distribution],
    maybeHotSpotDistribution: Option[Distribution],
    maybeFairChoiceRate: Option[Probability])
    extends Neighbourhood
{

    if (maybeSelectionSizeDistribution.isDefined) {
        val distribution = maybeSelectionSizeDistribution.get
        require(distribution.frequency(0) == 0)
        require(distribution.numberOfAlternatives >= 1)
        val searchVariables =
            neighbourhoods.foldLeft(mutable.HashSet.empty) {
                case (xs, neighbourhood) => xs.addAll(neighbourhood.searchVariables)
            }
        val neighbourhoodsAreDisjoint =
            searchVariables.size == neighbourhoods.iterator.map(_.searchVariables.size).sum
        require(neighbourhoodsAreDisjoint)
    }

    private val neighbourhoodSizeDistribution = Distribution(0, neighbourhoods.map(_.searchVariables.size))
    private val frequencyRestorer = new FrequencyRestorer(maybeSelectionSizeDistribution.map(_.size).getOrElse(2) - 2)
    private val lastProposals = new mutable.AnyRefMap[Neighbourhood, Move](neighbourhoods.size)

    override def searchVariables = neighbourhoods.iterator.flatMap(_.searchVariables).toSet

    override def children = neighbourhoods

    override def nextMove = {
        val useSizeDistribution =
            maybeHotSpotDistribution.isEmpty ||
                maybeHotSpotDistribution.get.volume == 0 ||
                (maybeFairChoiceRate.isDefined && randomGenerator.nextDecision(maybeFairChoiceRate.get))
        val priorityDistribution =
            if useSizeDistribution then neighbourhoodSizeDistribution else maybeHotSpotDistribution.get
        val m =
            if maybeSelectionSizeDistribution.isEmpty
            then 1
            else min(maybeSelectionSizeDistribution.get.nextIndex(randomGenerator), priorityDistribution.numberOfAlternatives)
        lastProposals.clear()
        if (m == 1) {
            val i = priorityDistribution.nextIndex(randomGenerator)
            val neighbourhood = neighbourhoods(i)
            val move = neighbourhood.nextMove
            lastProposals.update(neighbourhood, move)
            move
        } else {
            val result = new BulkMove(space.nextMoveId())
            scoped(frequencyRestorer) {
                for (i <- priorityDistribution.nextIndices(randomGenerator, m, frequencyRestorer)) {
                    val neighbourhood = neighbourhoods(i)
                    val move = neighbourhood.nextMove
                    lastProposals.update(neighbourhood, move)
                    result ++= move.effects
                }
            }
            result
        }
    }

    override def commit(move: Move) = {
        for ((neighbourhood, move) <- lastProposals) {
            neighbourhood.commit(move)
        }
    }

}
