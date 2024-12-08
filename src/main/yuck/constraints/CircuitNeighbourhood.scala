package yuck.constraints

import scala.collection.*

import yuck.core.*

/**
 * This neighbourhood can be used to maintain any ''circuit'' constraint.
 *
 * It swaps nodes and segments.
 *
 * @author Michael Marte
 */
final class CircuitNeighbourhood
    (space: Space,
     succ: immutable.IndexedSeq[IntegerVariable], offset: Int,
     randomGenerator: RandomGenerator)
    extends Neighbourhood
{

    private val debug = false

    private val n = succ.size
    private val now = space.searchState

    require(n > 2)
    require(succ.toSet.size == n)
    require(succ.forall(! space.isChannelVariable(_)))
    require(succ.forall(_.domain.isFinite))
    require(succ.forall(_.hasValidValue(space.searchState)))
    require(Circuit.isHamiltonianCircuit(succ, offset, space.searchState))

    override def searchVariables = succ.iterator.filterNot(_.domain.isSingleton).toSet

    override def children = Nil

    private def computeCurrentCycle: Array[Int] = {
        val cycle = new Array[Int](n)
        var i = offset
        var j = 0
        while {
            cycle.update(j, i - offset)
            i = now.value(succ(i - offset)).toInt
            j += 1
            i != offset
        } do ()
        assert(cycle.size == n)
        cycle
    }

    private val effects = new mutable.ArrayBuffer[MoveEffect[IntegerValue]](3) {
        override def clear() = {
            // No need to clear the underlying array!
            size0 = 0
        }
    }

    private val currentCycle: Array[Int] = computeCurrentCycle

    private def isFixed(i: Int): Boolean = succ(currentCycle((i + n) % n)).domain.isSingleton

    private def edgeExists(i: Int, j: Int): Boolean = {
        val x = succ(currentCycle((i + n) % n))
        val a = IntegerValue(currentCycle((j + n) % n) + offset)
        x.domain.contains(a)
    }

    private def link(i: Int, j: Int): Unit = {
        val x = succ(currentCycle((i + n) % n))
        val a = IntegerValue(currentCycle((j + n) % n) + offset)
        val effect = x.reuseableEffect
        effect.a = a
        effects += effect
    }

    private abstract class CircuitMove extends Move(space.nextMoveId()) {
        final override val effects = CircuitNeighbourhood.this.effects
        final override def toString = "%s(%s)".format(getClass.getSimpleName, effects.mkString(", "))
    }
    private case class NodeSwap(i: Int, j: Int) extends CircuitMove
    private case class SegmentSwap(i: Int, j: Int, k: Int) extends CircuitMove
    private case class PathReversal(i: Int, j: Int) extends CircuitMove
    private case class EmptyMove() extends CircuitMove

    private def nodeSwaps: Iterator[Move] = {
        for {
            i <- randomGenerator.lazyShuffle(Range(0, n))
            if ! isFixed(i)
            j <- randomGenerator.lazyShuffle(Range(i + 1, i + n - 1))
            if edgeExists(i - 1, j) && edgeExists(j, if (j == i + 1) i else i + 1) &&
               edgeExists(j - 1, i) && edgeExists(i, j + 1)
        } yield {
            link(i - 1, j)
            if (j == i + 1) {
                link(j, i)
            } else {
                link(j, i + 1)
                link(j - 1, i)
            }
            link(i, j + 1)
            NodeSwap(i, j)
        }
    }

    private def performNodeSwap(swap: NodeSwap): Unit = {
        val NodeSwap(i, j) = swap
        val tmp = currentCycle(i % n)
        currentCycle.update(i % n, currentCycle(j % n))
        currentCycle.update(j % n, tmp)
    }

    // see Aarts & Lenstra, Local search in combinatorial optimization, p. 230
    private def segmentSwaps: Iterator[Move] = {
        for {
            i <- randomGenerator.lazyShuffle(Range(0, n))
            if ! isFixed(i)
            k <- randomGenerator.lazyShuffle(Range(i + 2, i + min(100, n) - 1))
            if edgeExists(k, i + 1)
            j <- randomGenerator.lazyShuffle(Range(i + 1, k))
            if edgeExists(j, k + 1) && edgeExists(i, j + 1)
        } yield {
            link(i, j + 1)
            link(k, i + 1)
            link(j, k + 1)
            SegmentSwap(i, j, k)
        }
    }

    private def performSegmentSwap(swap: SegmentSwap): Unit = {
        val SegmentSwap(i, j, k) = swap
        // We consider three segments:
        // (1) s1 = ]k, i]
        // (2) s2 = ]i, j]
        // (3) s3 = ]j, k]
        // There is no need to touch s1 but we have to swap s2 and s3.
        // Except for the special case of s2 and s3 having the same size, we cannot just swap them in place.
        // Instead we copy the larger segment and recompute the other from the current search state.
        // (This way we reduce the number of value lookups.)
        val m2 = j - i
        val m3 = k - j
        if (m3 >= m2) {
            var l = i + 1
            // copy s3
            while (l <= i + m3) {
                currentCycle.update(l % n, currentCycle((l + m2) % n))
                l += 1
            }
            // recompute s2
            while (l <= k) {
                currentCycle.update(l % n, now.value(succ(currentCycle((l - 1) % n))).toInt - offset)
                l += 1
            }
        } else {
            var l = j
            // copy s2
            while (l > i) {
                currentCycle.update((l + m3) % n, currentCycle(l % n))
                l -= 1
            }
            // recompute s3
            l += 1
            while (l <= i + m3) {
                currentCycle.update(l % n, now.value(succ(currentCycle((l - 1) % n))).toInt - offset)
                l += 1
            }
        }
    }

    def findReversiblePath(from: Int, maxPathLength: Int): Option[Int] = {
        require(maxPathLength <= n)
        var reversible = true
        var i = from
        var m = 0
        while (reversible && m < maxPathLength) {
            reversible = edgeExists(i + 1, i)
            if (reversible) {
                m += 1
                i += 1
            }
        }
        var success = false
        var j = from + m
        while (! success && j > from + 2) {
            success = edgeExists(from, j) && edgeExists(from + 1, j + 1)
            j -= 1
        }
        if (success) Some(j + 1) else None
    }

    private def pathReversals: Iterator[Move] =
        for {
            i <- randomGenerator.lazyShuffle(Range(0, n))
            maxPathLength = max(3, randomGenerator.nextInt(min(n - 1, 25)) + 1)
            maybeJ = findReversiblePath(i, maxPathLength)
            if maybeJ.isDefined
        } yield {
            val j = maybeJ.get
            assert(j - i < n)
            link(i, j)
            var k = j
            while (k > i + 1) {
                link(k, k - 1)
                k -= 1
            }
            link(i + 1, j + 1)
            PathReversal(i, j)
        }

    private def performPathReversal(pathReversal: PathReversal): Unit = {
        val PathReversal(i, j) = pathReversal
        var k = i + 1
        var l = j
        while (k < l) {
            val tmp = currentCycle(k % n)
            currentCycle.update(k % n, currentCycle(l % n))
            currentCycle.update(l % n, tmp)
            k += 1
            l -= 1
        }
    }

    override def nextMove = {
        effects.clear()
        val decision = randomGenerator.nextInt(10)
        val maybeMove = {
            if (decision < 1) pathReversals.nextOption().orElse(segmentSwaps.nextOption().orElse(nodeSwaps.nextOption()))
            else if (decision < 6) segmentSwaps.nextOption().orElse(nodeSwaps.nextOption())
            else nodeSwaps.nextOption().orElse(segmentSwaps.nextOption())
        }
        if (maybeMove.isDefined) {
            val move = maybeMove.get
            if (debug) {
                assert(move.effects.eq(effects))
                for (effect <- effects) {
                    assert(effect.x.domain.contains(effect.a))
                }
                val now = space.searchState
                val after = new MoveSimulator(now, move)
                require(Circuit.isHamiltonianCircuit(succ, offset, after))
            }
            move
        } else {
            EmptyMove()
        }
    }

    override def commit(move: Move) = {
        move match {
            case swap: NodeSwap => performNodeSwap(swap)
            case swap: SegmentSwap => performSegmentSwap(swap)
            case exchange: PathReversal => performPathReversal(exchange)
            case EmptyMove() =>
        }
    }

}
