package yuck.core

import scala.collection._

/**
 * Turns the given collection of neighbourhoods into a neighbourhood.
 *
 * To generate a move, one of the neighbourhoods is chosen and asked for a move.
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
    neighbourhoods: immutable.IndexedSeq[Neighbourhood],
    randomGenerator: RandomGenerator,
    maybeHotSpotDistribution: Option[Distribution],
    maybeFairChoiceRate: Option[Probability])
    extends Neighbourhood
{
    private var lastNeighbourhood: Neighbourhood = null
    override def searchVariables = neighbourhoods.iterator.flatMap(_.searchVariables).toSet
    override def children = neighbourhoods
    private val sizeDistribution = Distribution(neighbourhoods.size)
    (0 until neighbourhoods.size).foreach(i => sizeDistribution.setFrequency(i, neighbourhoods(i).searchVariables.size))
    override def nextMove = {
        val useSizeDistribution =
            maybeHotSpotDistribution.isEmpty ||
            maybeHotSpotDistribution.get.volume == 0 ||
            (maybeFairChoiceRate.isDefined && randomGenerator.nextDecision(maybeFairChoiceRate.get))
        val priorityDistribution = if (useSizeDistribution) sizeDistribution else maybeHotSpotDistribution.get
        val i = priorityDistribution.nextIndex(randomGenerator)
        lastNeighbourhood = neighbourhoods(i)
        lastNeighbourhood.nextMove
    }
    override def commit(move: Move) = {
        lastNeighbourhood.commit(move)
        lastNeighbourhood = null
    }
}
