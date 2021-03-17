package yuck.constraints

import scala.collection._

import yuck.core._

/**
 * This 3-opt-like neighbourhood can be used to maintain any ''circuit'' constraint.
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
    private def value(x: IntegerVariable) = space.searchState.value(x)

    require(n > 2)
    require(succ.toSet.size == n)
    require(succ.forall(! space.isChannelVariable(_)))
    require(succ.forall(_.domain.isFinite))
    require(succ.forall(x => x.domain.contains(value(x))))
    require(CircuitTracker.isHamiltonianCircuit(succ, offset, space.searchState))

    private val effects = Vector.fill(3){new ReusableMoveEffect[IntegerValue]}
    private val swaps = for (n <- 1 to 3) yield effects.take(n)
    private def succeed(n: Int): Move = new ChangeValues[IntegerValue](space.nextMoveId, swaps(n - 1))
    private def fail: Move = new ChangeValues[IntegerValue](space.nextMoveId, Nil)

    override def searchVariables = succ.iterator.filterNot(_.domain.isSingleton).toSet

    override def children = Nil

    // see Aarts & Lenstra, Local search in combinatorial optimization, p. 230
    override def nextMove = {
        val cycle = new mutable.ArrayBuffer[IntegerVariable](n)
        var i = offset
        do {
            cycle += succ(i - offset)
            i = value(succ(i - offset)).value
        } while (i != offset)
        assert(cycle.size == n)
        val candidates =
            for {
                i <- randomGenerator.lazyShuffle(Range(0, n))
                x = cycle(i)
                if ! x.domain.isSingleton
                a = value(x)
                k <- randomGenerator.lazyShuffle(Range(i + 2, i + n - 1))
                z = cycle(k % n)
                if z.domain.contains(a)
                c = value(z)
                j <- randomGenerator.lazyShuffle(Range(i + 1, k))
                y = cycle(j % n)
                if y.domain.contains(c) && x.domain.contains(value(y))
            } yield {
                (x, y, z)
            }
        if (candidates.hasNext) {
            val (x, y, z) = candidates.next()
            val (a, b, c) = (value(x), value(y), value(z))
            if (debug) {
                assert(x.domain.contains(b))
                assert(y.domain.contains(c))
                assert(z.domain.contains(a))
            }
            effects(0).set(x, b)
            effects(1).set(y, c)
            effects(2).set(z, a)
            val move = succeed(3)
            if (debug) {
                val now = space.searchState
                val after = new MoveSimulator(now, move)
                require(CircuitTracker.isHamiltonianCircuit(succ, offset, after))
            }
            move
        } else {
            fail
        }
    }

}
