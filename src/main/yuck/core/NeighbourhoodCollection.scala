package yuck.core

import scala.collection._

/**
 * Turns the given collection of neighbourhoods into a neighbourhood.
 *
 * To generate a move, one of the neighbourhoods is chosen and asked for a move.
 *
 * Neighbourhood selection can happen in two ways:
 * In fair mode, all neighbourhoods are equally likely to get chosen while
 * in unfair mode the selection probability may be skewed in some way.
 *
 * To facilitate unfair choice, a so-called hot-spot distribution has to be given.
 *
 * With unfair choice enabled, the probability of fair choice comes into play.
 *
 * The implementation of fair neighbourhood selection is based on the neighbourhood sizes:
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
    hotSpotDistribution: Distribution,
    probabilityOfFairChoiceInPercent: Int)
    extends Neighbourhood
{
    require((0 to 100).contains(probabilityOfFairChoiceInPercent))
    override def searchVariables = neighbourhoods.toIterator.map(_.searchVariables).flatten.toSet
    private val sizeDistribution = DistributionFactory.createDistribution(neighbourhoods.size)
    (0 until neighbourhoods.size).foreach(i => sizeDistribution.setFrequency(i, neighbourhoods(i).searchVariables.size))
    override def nextMove = {
        val useSizeDistribution =
            hotSpotDistribution == null ||
            hotSpotDistribution.volume == 0 ||
            probabilityOfFairChoiceInPercent == 100 ||
            (probabilityOfFairChoiceInPercent > 0 && randomGenerator.nextInt(100) < probabilityOfFairChoiceInPercent)
        val priorityDistribution = if (useSizeDistribution) sizeDistribution else hotSpotDistribution
        val i = priorityDistribution.nextIndex(randomGenerator)
        neighbourhoods(i).nextMove
    }
}
