package yuck.core

import scala.collection._
import scala.math._

/**
 * Generates random moves of random size.
 *
 * For each chosen variable, a value that differs from its current value is
 * randomly chosen from its domain.
 *
 * Choosing the number of variables involved in a move is guided by the given
 * move-size distribution.
 *
 * Variable choice can happen in two ways:
 * In fair mode, all variables are equally likely to occur in a move while
 * in unfair mode the selection probability may be skewed in some way.
 *
 * To facilitate unfair variable choice, a so-called hot-spot distribution has to be given.
 *
 * In unfair mode, the probability of fair variable choice comes into play.
 *
 * Falls back to fair mode when the given hot-spot distribution has zero volume.
 *
 * @author Michael Marte
 */
final class RandomReassignmentGenerator
    (space: Space,
     xs: immutable.IndexedSeq[AnyVariable],
     randomGenerator: RandomGenerator,
     moveSizeDistribution: Distribution,
     maybeHotSpotDistribution: Option[Distribution],
     maybeFairVariableChoiceRate: Option[Probability])
    extends Neighbourhood
{

    private val n = xs.size
    require(n > 0)
    require(n == xs.toSet.size)

    require(xs.forall(space.isSearchVariable))
    require(xs.forall(_.domain.isFinite))

    require(moveSizeDistribution.frequency(0) == 0)
    require(moveSizeDistribution.volume > 0)

    private val uniformDistribution = DistributionFactory.createDistribution(n)
    (0 until n).foreach(i => uniformDistribution.setFrequency(i, 1))
    require(uniformDistribution.volume > 0)
    private val s = moveSizeDistribution.size
    private val effectsByMoveSize = for (n <- 1 until s) yield new Array[AnyEffect](n)
    private val frequencyRestorers = for (i <- 1 until s) yield new FrequencyRestorer
    @inline private def fillEffect(effects: Array[AnyEffect], i: Int, x: AnyVariable): Unit = {
        effects.update(i, x.nextRandomEffect(space, randomGenerator))
    }

    override def searchVariables = xs.toSet

    override def children = Nil

    override def nextMove = {
        val useUniformDistribution =
            maybeHotSpotDistribution.isEmpty ||
            maybeHotSpotDistribution.get.volume == 0 ||
            (maybeFairVariableChoiceRate.isDefined && randomGenerator.nextDecision(maybeFairVariableChoiceRate.get))
        val priorityDistribution = if (useUniformDistribution) uniformDistribution else maybeHotSpotDistribution.get
        val m =
            scala.math.min(
                moveSizeDistribution.nextIndex(randomGenerator),
                priorityDistribution.numberOfAlternatives)
        assert(m > 0)
        val effects = effectsByMoveSize(m - 1)
        var i = 0
        if (useUniformDistribution && m < 4) {
            val i = randomGenerator.nextInt(n)
            fillEffect(effects, 0, xs(i))
            if (m > 1) {
                val j = {
                    val k = randomGenerator.nextInt(n - 1)
                    if (k < i) k else k + 1
                }
                fillEffect(effects, 1, xs(j))
                if (m > 2) {
                    val k = {
                        var l = randomGenerator.nextInt(n - 2)
                        if (l < min(i, j)) l else if (l > max(i, j) - 2) l + 2 else l + 1
                    }
                    fillEffect(effects, 2, xs(k))
                }
            }
        } else {
            while (i < m && priorityDistribution.volume > 0) {
                val j = priorityDistribution.nextIndex(randomGenerator)
                fillEffect(effects, i, xs(j))
                if (i < m - 1) {
                    frequencyRestorers(i).store(j, priorityDistribution.frequency(j))
                    priorityDistribution.setFrequency(j, 0)
                }
                i += 1
            }
            if (m > 1) {
                i = 0
                while (i < m - 1) {
                    frequencyRestorers(i).restore(priorityDistribution)
                    i += 1
                }
            }
        }
        val result = new ChangeAnyValues(space.nextMoveId, effects)
        result
    }

}
