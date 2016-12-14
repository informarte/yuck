package yuck.core

import scala.collection._
import scala.math._

/**
 * Generates random moves of random size.
 *
 * Chooses a sequence of variables and proposes to swap the variable's values
 * pair-wise.
 *
 * The variables are constrained to have equal domains such that swapping values
 * is a safe operation.
 *
 * Choosing the number of variables involved in a move is guided by the given
 * move-size distribution.
 *
 * Variable selection can happen in two ways:
 * In fair mode, all variables are equally likely to occur in a move while
 * in unfair mode the selection probability may be skewed in some way.
 *
 * To facilitate unfair choice, a so-called hot-spot distribution has to be given.
 *
 * With unfair choice enabled, the probability of fair choice comes into play.
 *
 * Falls back to fair mode when the given hot-spot distribution has zero volume.
 *
 * @author Michael Marte
 */
final class RandomCircularSwapGenerator
    [Value <: AnyValue]
    (space: Space,
     xs: immutable.IndexedSeq[Variable[Value]],
     randomGenerator: RandomGenerator,
     moveSizeDistribution: Distribution,
     hotSpotDistribution: Distribution,
     probabilityOfFairChoiceInPercent: Int)
    extends MoveGenerator
{

    private val n = xs.size
    require(n > 1)
    require(n == xs.toSet.size)
    require(xs.forall(_.domain.isFinite))
    require(xs.forall(! _.isParameter))
    for (i <- 1 until n) {
        require(xs(i - 1).domain == xs(i).domain)
    }
    require(moveSizeDistribution.frequency(0) == 0)
    require(moveSizeDistribution.volume > 0)
    require((0 to 100).contains(probabilityOfFairChoiceInPercent))
    private val uniformDistribution = DistributionFactory.createDistribution(n)
    (0 until n).foreach(i => uniformDistribution.setFrequency(i, 1))
    private val s = moveSizeDistribution.size
    private val effects = for (i <- 1 until s) yield new ReusableEffect[Value]
    private val swaps = for (n <- 1 until s) yield effects.take(n)
    private val frequencyRestorers = for (i <- 1 until s) yield new FrequencyRestorer
    @inline private def fillEffect(effect: ReusableEffect[Value], x: Variable[Value]) {
        effect.x = x
        effect.a = space.searchState.value(x)
    }
    private def swapValues(swap: IndexedSeq[ReusableEffect[Value]]) {
        // x <- y <- z <- x
        val m = swap.size
        val a0 = swap(0).a
        var i = 1
        while (i < m) {
            swap(i - 1).a = swap(i).a
            i += 1
        }
        swap(m - 1).a = a0
    }

    override def searchVariables = xs

    override def nextMove = {
        val useUniformDistribution =
            hotSpotDistribution == null ||
            hotSpotDistribution.volume == 0 ||
            probabilityOfFairChoiceInPercent == 100 ||
            (probabilityOfFairChoiceInPercent > 0 && randomGenerator.nextInt(100) < probabilityOfFairChoiceInPercent)
        val priorityDistribution = if (useUniformDistribution) uniformDistribution else hotSpotDistribution
        val m =
            scala.math.min(
                scala.math.max(2, scala.math.min(moveSizeDistribution.nextIndex(randomGenerator), n)),
                priorityDistribution.numberOfAlternatives)
        assert(m > 1)
        val swap = swaps(m - 1)
        if (useUniformDistribution && m < 4) {
            val i = randomGenerator.nextInt(n)
            fillEffect(swap(0), xs(i))
            if (m > 1) {
                val j = {
                    val k = randomGenerator.nextInt(n - 1)
                    if (k < i) k else k + 1
                }
                fillEffect(swap(1), xs(j))
                if (m > 2) {
                    val k = {
                        var l = randomGenerator.nextInt(n - 2)
                        if (l < min(i, j)) l else if (l > max(i, j) - 2) l + 2 else l + 1
                    }
                    fillEffect(swap(2), xs(k))
                }
            }
            swapValues(swap)
        } else {
            var i = 0
            while (i < m) {
                val j = priorityDistribution.nextIndex(randomGenerator)
                fillEffect(swap(i), xs(j))
                if (i < m - 1) {
                    frequencyRestorers(i).store(j, priorityDistribution.frequency(j))
                    priorityDistribution.setFrequency(j, 0)
                }
                i += 1
            }
            swapValues(swap)
            if (m > 1) {
                var i = 0
                while (i < m - 1) {
                    frequencyRestorers(i).restore(priorityDistribution)
                    i += 1
                }
            }
        }
        new ChangeValues(space.moveIdFactory.nextId, swap)
    }

}
