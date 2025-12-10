package yuck.core

import scala.collection.*

import yuck.util.arm.scoped

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
 */
final class RandomReassignmentGenerator
    (override protected val space: Space,
     xs: immutable.IndexedSeq[AnyVariable],
     randomGenerator: RandomGenerator,
     moveSizeDistribution: Distribution = Distribution(1, List(1)),
     maybeHotSpotDistribution: Option[Distribution] = None,
     maybeFairVariableChoiceRate: Option[Probability] = None)
    extends Neighbourhood
{

    private val n = xs.size
    require(n > 0)
    require(n == xs.toSet.size)

    require(xs.forall(space.isSearchVariable))
    require(xs.forall(_.domain.isFinite))

    require(moveSizeDistribution.frequency(0) == 0)
    require(moveSizeDistribution.volume > 0)

    private val uniformDistribution = Distribution(0, Vector.fill(n)(1))
    private val effects = new mutable.ArrayBuffer[AnyMoveEffect](n)
    private val frequencyRestorer= new FrequencyRestorer(moveSizeDistribution.size - 2)

    private def addEffect(x: AnyVariable): Unit = {
        effects += x.nextRandomMoveEffect(space, randomGenerator)
    }

    override def searchVariables = xs.toSet

    override def children = Nil

    override def nextMove() = {
        val useUniformDistribution =
            maybeHotSpotDistribution.isEmpty ||
                maybeHotSpotDistribution.get.volume == 0 ||
                (maybeFairVariableChoiceRate.isDefined && randomGenerator.nextDecision(maybeFairVariableChoiceRate.get))
        val priorityDistribution = if useUniformDistribution then uniformDistribution else maybeHotSpotDistribution.get
        val m = min(moveSizeDistribution.nextIndex(randomGenerator), priorityDistribution.numberOfAlternatives)
        assert(m > 0)
        effects.clear()
        if useUniformDistribution && m < 4 then {
            val i = randomGenerator.nextInt(n)
            addEffect(xs(i))
            if m > 1 then {
                val j = {
                    val k = randomGenerator.nextInt(n - 1)
                    if k < i then k else k + 1
                }
                addEffect(xs(j))
                if m > 2 then {
                    val k = {
                        val l = randomGenerator.nextInt(n - 2)
                        if l < min(i, j) then l else if l > max(i, j) - 2 then l + 2 else l + 1
                    }
                    addEffect(xs(k))
                }
            }
        } else scoped(frequencyRestorer) {
            priorityDistribution.nextIndices(randomGenerator, m, frequencyRestorer).foreach(i => addEffect(xs(i)))
        }
        val result = new ChangeAnyValues(space.nextMoveId(), effects)
        result
    }

    override def perturb(perturbationProbability: Probability) = {
        val move = new BulkMove(space.nextMoveId())
        for x <- xs do {
            if randomGenerator.nextDecision(perturbationProbability) then {
                move += x.nextRandomMoveEffect(space, randomGenerator)
            }
        }
        space.consult(move)
        space.commit(move)
    }

}
