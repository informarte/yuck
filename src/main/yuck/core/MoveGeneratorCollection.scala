package yuck.core

import scala.collection._

/**
 * Turns the given collection of move generators into a move generator.
 *
 * To generate a move, one of the given generators is chosen and asked for a move.
 *
 * Generator selection can happen in two ways:
 * In fair mode, all generators are equally likely to get chosen while
 * in unfair mode the selection probability may be skewed in some way.
 *
 * To facilitate unfair choice, a so-called hot-spot distribution has to be given.
 *
 * With unfair choice enabled, the probability of fair choice comes into play.
 *
 * The implementation of fair generator selection is based on the generator sizes:
 * The probability of choosing a generator is proportional to the number of its variables.
 *
 * Falls back to fair mode when the given hot-spot distribution has zero volume.
 *
 * @author Michael Marte
 */
final class MoveGeneratorCollection(
    moveGenerators: immutable.IndexedSeq[MoveGenerator],
    randomGenerator: RandomGenerator,
    hotSpotDistribution: Distribution,
    probabilityOfFairChoiceInPercent: Int)
    extends MoveGenerator
{
    require((0 to 100).contains(probabilityOfFairChoiceInPercent))
    override val xs = moveGenerators.map(_.xs).flatten
    private val sizeDistribution = DistributionFactory.createDistribution(moveGenerators.size)
    (0 until moveGenerators.size).foreach(i => sizeDistribution.setFrequency(i, moveGenerators(i).xs.size))
    override def nextMove = {
        val useSizeDistribution =
            hotSpotDistribution == null ||
            hotSpotDistribution.volume == 0 ||
            probabilityOfFairChoiceInPercent == 100 ||
            (probabilityOfFairChoiceInPercent > 0 && randomGenerator.nextInt(100) < probabilityOfFairChoiceInPercent)
        val priorityDistribution = if (useSizeDistribution) sizeDistribution else hotSpotDistribution
        val i = priorityDistribution.nextIndex(randomGenerator)
        moveGenerators(i).nextMove
    }
}
