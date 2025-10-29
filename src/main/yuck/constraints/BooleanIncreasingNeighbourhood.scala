package yuck.constraints

import scala.collection.immutable

import yuck.core.*

/**
 * This neighbourhood can be used to maintain non-strict ''increasing'' constraints
 * over Boolean variables.
 *
 * @author Michael Marte
 */
final class BooleanIncreasingNeighbourhood
    (override protected val space: Space,
     xs: immutable.IndexedSeq[BooleanVariable],
     randomGenerator: RandomGenerator,
     moveSizeDistribution: Distribution)
    extends Neighbourhood
{

    private val n = xs.size

    require(n > 1)
    require(xs.toSet.size == n)
    require(xs.exists(space.isSearchVariable))
    require(xs.forall(_.hasValidValue(space.searchState)))
    require((0 until n - 1).forall(
        i => space.searchState.value(xs(i)).truthValue <= space.searchState.value(xs(i + 1)).truthValue))

    require(moveSizeDistribution.frequency(0) == 0)
    require(moveSizeDistribution.volume > 0)

    override def searchVariables = xs.iterator.filterNot(_.domain.isSingleton).toSet

    override def children = Nil

    // All variables xs(i) with i >= currentBoundary have true assigned, all other variables have false assigned.
    private var currentBoundary =
        Range(0, n).view.filter(i => space.searchState.value(xs(i)).truthValue).headOption.getOrElse(n)
    private var futureBoundary = 0

    private def shiftLeft(move: BulkMove, m: Int): BulkMove = {
        var i = 0
        while (i < m) {
            val effect = xs(currentBoundary - i - 1).reuseableEffect
            effect.a = True
            move += effect
            i += 1
        }
        futureBoundary = currentBoundary - m
        move
    }

    private def shiftRight(move: BulkMove, m: Int): BulkMove = {
        var i = 0
        while (i < m) {
            val effect = xs(currentBoundary + i).reuseableEffect
            effect.a = False
            move += effect
            i += 1
        }
        futureBoundary = currentBoundary + m
        move
    }

    override def nextMove() = {
        val move = new BulkMove(space.nextMoveId())
        val m = min(moveSizeDistribution.nextIndex(randomGenerator), max(currentBoundary, n - currentBoundary))
        val canShiftLeft = currentBoundary >= m
        val canShiftRight = n - currentBoundary >= m
        (canShiftLeft, canShiftRight) match {
            case (true, true) => if randomGenerator.nextDecision() then shiftLeft(move, m) else shiftRight(move, m)
            case (true, false) => shiftLeft(move, m)
            case (false, true) => shiftRight(move, m)
            case (false, false) => move
        }
    }

    override def commit(move: Move) = {
        currentBoundary = futureBoundary
    }

}
