package yuck.constraints

import scala.annotation.tailrec
import scala.collection.*

import yuck.core.*
import yuck.util.arm.scoped

/**
 * This neighbourhood can be used to maintain strict and non-strict ''increasing'' constraints
 * over integer variables.
 *
 * @author Michael Marte
 */
final class IntegerIncreasingNeighbourhood
    (override protected val space: Space,
     xs: immutable.IndexedSeq[IntegerVariable],
     strict: Boolean,
     randomGenerator: RandomGenerator,
     moveSizeDistribution: Distribution,
     maybeHotSpotDistribution: Option[Distribution],
     maybeFairVariableChoiceRate: Option[Probability])
    extends Neighbourhood
{

    private val n = xs.size

    require(n > 1)
    require(xs.toSet.size == n)
    require(xs.exists(space.isSearchVariable))
    require(xs.forall(_.domain.isFinite))
    require(xs.forall(_.hasValidValue(space.searchState)))
    require((0 until n - 1).forall(i => {
        val a = space.searchState.value(xs(i))
        val b = space.searchState.value(xs(i + 1))
        if strict then a < b else a <= b
    }))

    require(moveSizeDistribution.frequency(0) == 0)
    require(moveSizeDistribution.volume > 0)

    override def searchVariables = xs.iterator.filterNot(_.domain.isSingleton).toSet

    override def children = Nil

    private val offset = if strict then One else Zero

    // This distribution was tuned to solve oocsp_racks instances.
    // The focus is on small shifts because bigger shifts tend to impair performance.
    private val distanceDistribution = Distribution(1, Vector(64, 32, 16, 8, 4, 2, 1))

    private val uniformDistribution =
        Distribution(0, Vector.tabulate(n)(i => if space.isProblemParameter(xs(i)) then 0 else 1))

    private val frequencyRestorer = new FrequencyRestorer(moveSizeDistribution.size - 2)

    override def nextMove() = {
        nextMove(true)
    }

    private def nextMove(biased: Boolean) = {
        val useUniformDistribution =
            ! biased ||
                maybeHotSpotDistribution.isEmpty ||
                maybeHotSpotDistribution.get.volume == 0 ||
                (maybeFairVariableChoiceRate.isDefined && randomGenerator.nextDecision(maybeFairVariableChoiceRate.get))
        val priorityDistribution = if useUniformDistribution then uniformDistribution else maybeHotSpotDistribution.get
        val m = min(moveSizeDistribution.nextIndex(randomGenerator), priorityDistribution.numberOfAlternatives)
        val move = new BulkMove(space.nextMoveId())
        val after = new MoveSimulator(space.searchState, move)
        val result =
            if m == 1
            then augmentMove(move, after, priorityDistribution.nextIndex(randomGenerator))
            else scoped(frequencyRestorer) {
                priorityDistribution
                    .nextIndices(randomGenerator, m, frequencyRestorer)
                    .foldLeft(move)((move, i) => if move.size >= m then move else augmentMove(move, after, i))
            }
        result
    }

    // Tries to shift the value of xs(i) by a random distance (see distanceDistribution).
    // Makes room by shifting the values of other variables, too.
    // searchState is expected to reflect changes made to move.
    private def augmentMove(move: BulkMove, searchState: SearchState, i: Int): BulkMove = {
        if (move.involves(xs(i))) {
            move
        } else {

            // Returns true iff the value assignment could be changed such that xs(i) takes a value not greater than ub.
            @tailrec
            def canShiftLeft(i: Int, ub: IntegerValue): Boolean =
                ! move.involves(xs(i)) && {
                    val rem = xs(i).domain.boundFromAbove(ub)
                    (! rem.isEmpty &&
                        (i == 0 || searchState.value(xs(i - 1)) + offset <= rem.ub || canShiftLeft(i - 1, rem.ub - offset)))
                }

            // Returns true iff the value assignment could be changed such that xs(i) takes a value not smaller than lb.
            @tailrec
            def canShiftRight(i: Int, lb: IntegerValue): Boolean =
                ! move.involves(xs(i)) && {
                    val rem = xs(i).domain.boundFromBelow(lb)
                    (! rem.isEmpty &&
                        (i == n - 1 || rem.lb + offset <= searchState.value(xs(i + 1)) || canShiftRight(i + 1, rem.lb + offset)))
                }


            // Changes the value assignment such that xs(i) takes a value not greater than ub.
            // Assumes that canShiftLeft(i, ub) is true.
            def shiftLeft(i: Int, ub: IntegerValue): Unit = {
                require(i >= 0)
                val rem = xs(i).domain.boundFromAbove(ub)
                if (i > 0 && searchState.value(xs(i - 1)) + offset > rem.ub) {
                    shiftLeft(i - 1, rem.ub - offset)
                }
                val effect = xs(i).reuseableEffect
                effect.a = rem.ub
                move += effect
            }

            // Changes the value assignment such that xs(i) takes a value not smaller than lb.
            // Assumes that canShiftRight(i, lb) is true.
            def shiftRight(i: Int, lb: IntegerValue): Unit = {
                require(i < n)
                val rem = xs(i).domain.boundFromBelow(lb)
                if (i < n - 1 && rem.lb + offset > searchState.value(xs(i + 1))) {
                    shiftRight(i + 1, rem.lb + offset)
                }
                val effect = xs(i).reuseableEffect
                effect.a = rem.lb
                move += effect
            }

            // Tries to shift the value of xs(i) by at least the given distance.
            // Recurses with distance / 2 when the attempt failed.
            @tailrec
            def shift(i: Int, distance: Int): Unit = {
                val a = searchState.value(xs(i))
                val b = IntegerValue(a.value - distance)
                val c = IntegerValue(a.value + distance)
                if (randomGenerator.nextDecision()) {
                    // first try left, then right
                    if canShiftLeft(i, b)
                    then shiftLeft(i, b)
                    else if canShiftRight(i, c)
                    then shiftRight(i, c)
                    else if distance > 1
                    then shift(i, distance / 2)
                } else {
                    // first try right, then left
                    if canShiftRight(i, c)
                    then shiftRight(i, c)
                    else if canShiftLeft(i, b)
                    then shiftLeft(i, b)
                    else if distance > 1
                    then shift(i, distance / 2)
                }
            }

            shift(i, distanceDistribution.nextIndex(randomGenerator))
            move
        }
    }

    override def perturb(perturbationProbability: Probability): Unit = {
        val move = nextMove(false)
        space.consult(move)
        space.commit(move)
        commit(move)
    }

}
