package yuck.constraints

import scala.collection._
import scala.math._

import yuck.core._

/**
 * Implements the ''regular'' constraint as specified by MiniZinc.
 *
 * To understand the cost model, let q denote the last state (the state after processing
 * the last input, it may be failed) and let 0 <= i < n denote the position at which q was reached.
 * (If q != 0, then i = n - 1.)
 * We aim to compute the length l of the longest prefix that could be extended to an acceptable
 * sequence in order to return the ''backtrack distance'' d = n - l as a measure of constraint violation.
 * (The notion of ''backtrack distance'' derives from the imagination of a tree search with the
 * fixed variable ordering xs[1], xs[2], ...; it is not the Hamming distance but, obviously,
 * when d = 0, the sequence is acceptable.)
 * Actually we compute an upper bound l' of l and hence we return a lower bound d' on d.
 * We guarantee that d' = 0 iff the sequence is acceptable.
 * To compute d', we look for the latest state u from which an accepting state could presumably be reached.
 *
 * Presumably, d = d' under the following conditions:
 *  - The domains of the xs(i) allow for the transitions required to reach an accepting state from u.
 *  - Once in an accepting state, the DFA can stay in that state or oscillate between accepting states.
 *
 * @param xs Sequence of input variables (may contain channel and duplicate variables)
 * @param Q Number of states
 * @param S Number of inputs
 * @param delta State transition function 1..Q x 1..S -> 0..Q (0 is failed state)
 * @param q0 Start state in 1..Q
 * @param F Accepting states (subset of 1..Q)
 *
 * @author Michael Marte
 */
final class Regular
    (id: Id[Constraint], goal: Goal,
     xs: immutable.IndexedSeq[IntegerVariable],
     Q: Int,
     S: Int,
     delta: immutable.IndexedSeq[immutable.IndexedSeq[Int]],
     q0: Int,
     F: IntegerDomain,
     costs: BooleanVariable)
    extends Constraint(id, goal)
{

    require(Q > 0)
    require(S > 0)
    require(delta.size == Q)
    require(delta.forall(_.size == S))
    require(delta.forall(_.forall(q => q >= 0 && q <= Q)))
    require(q0 >= 1 && q0 <= Q)
    require(F.isSubsetOf(IntegerDomain.createRange(One, IntegerValue.get(Q))))

    private val n = xs.size
    private val hasDuplicateVariables = xs.toSet.size < n

    private val distancesToAcceptingState: immutable.IndexedSeq[Int] = {
        // We use the Floyd-Warshall algorithm to compute, for each q in Q, the minimum number of
        // transitions that are required to reach an accepting state from q.
        // (This solution has O(Q^3) complexity, so it does not really scale, but the Scala priority
        // queues do not support a proper Dijkstra implementation.)
        val d = Array.ofDim[Int](Q, Q)
        for (u <- 0 until Q)
            for (v <- 0 until Q)
                d(u)(v) = if (u == v) 0 else Int.MaxValue
        for (u <- 0 until Q)
            for (i <- 0 until S) {
                val v = delta(u)(i) - 1
                if (v > -1 /* ignore failed state */ && u != v) d(u)(v) = 1
            }
        for (k <- 0 until Q)
            for (i <- 0 until Q)
                for (j <- 0 until Q)
                    if (d(i)(k) < Int.MaxValue && d(k)(j) < Int.MaxValue && d(i)(j) > d(i)(k) + d(k)(j))
                        d(i)(j) = d(i)(k) + d(k)(j)
        for (i <- 0 until Q) yield F.values.map(f => d(i)(f.value - 1)).min
    }

    override def toString =
        "regular([%s], %d, %d, [%s], %d, %s, %s)".format(
            xs.mkString(", "), Q, S,
            delta.map(row => "[%s]".format(row.mkString(", "))).mkString(", "), q0, F, costs)

    override def inVariables = xs
    override def outVariables = List(costs)

    private val x2i: immutable.Map[AnyVariable, Int] =
        if (hasDuplicateVariables) null else xs.iterator.zipWithIndex.toMap[AnyVariable, Int]
    private val x2is: immutable.Map[AnyVariable, immutable.IndexedSeq[Int]] =
        if (hasDuplicateVariables) {
            xs
            .iterator
            .zipWithIndex
            .foldLeft(new mutable.HashMap[AnyVariable, mutable.Buffer[Int]]) {
                case (map, (x, i)) =>
                    val buf = map.getOrElseUpdate(x, new mutable.ArrayBuffer[Int])
                    buf += i
                    map
            }
            .map{case (x, buf) => (x, buf.toIndexedSeq)}
            .toMap
        } else {
            null
        }

    private val effects = List(new ReusableMoveEffectWithFixedVariable[BooleanValue](costs))
    private val effect = effects.head
    private var currentCosts = 0
    private var currentStates: immutable.IndexedSeq[Int] = null
    // The position at which the current sequence is recognized as invalid.
    // All states after this position are to be considered as failed!
    // (We do not maintain those states i.e. they may actually be greater than 0.)
    private var currentFailurePosition = 0
    private var futureCosts = 0
    private var futureStates: immutable.IndexedSeq[Int] = null
    private var futureFailurePosition = 0

    override def initialize(now: SearchState) = {
        currentStates = immutable.IndexedSeq[Int]() ++ (0 until n).map(i => 0)
        var i = 0
        var q = q0
        while (i < n && q > 0) {
            val a = now.value(xs(i)).value
            q = if (a >= 1 && a <= S) delta(q - 1)(a - 1) else 0
            if (q > 0 && distancesToAcceptingState(q - 1) > n - i - 1) q = 0
            currentStates = currentStates.updated(i, q)
            i += 1
        }
        i -= 1
        currentCosts = computeCosts(currentStates, i, q)
        currentFailurePosition = if (q == 0) i else n
        assert(currentCosts >= 0)
        effect.a = BooleanValue.get(currentCosts)
        effects
    }

    override def consult(before: SearchState, after: SearchState, move: Move) = {
        // Basically we proceed as follows:
        // 1. We find all the i where xs(i) was changed by the move and collect them in the ordered sequence is.
        //    However, we ignore those i after the current failure position.
        //    (This is ok because when we eventually pass that position, we will recompute all states.)
        // 2. We start from the first i in is (i = is(j) with j = 0) and compute the future states.
        //    We stop at the end of the sequence, when we computed the failed state, or when we have a found a
        //    fixed point.
        //    (However, we do not check for a fixed point after the current failure position because after
        //    that position all current states are to be considered as failed.)
        //    Whenever we pass the first i in is, we remove it from is (by increasing j).
        // 3. In case some i is left in is (j < m), we continue with step 2.
        val is = {
            val xs = move.involvedVariables.iterator
            val is0 = if (hasDuplicateVariables) xs.map(x2is).flatten else xs.map(x2i)
            is0.filter(_ <= currentFailurePosition).toBuffer.sorted
        }
        val m = is.size
        var j = 0
        futureStates = currentStates
        futureCosts = currentCosts
        futureFailurePosition = currentFailurePosition
        if (m > 0) {
            var i = 0
            var q = 0
            do {
                i = is(j)
                j += 1
                q = if (i == 0) q0 else futureStates(i - 1)
                do {
                    val a = after.value(xs(i)).value
                    q = if (a >= 1 && a <= S) delta(q - 1)(a - 1) else 0
                    if (q > 0 && distancesToAcceptingState(q - 1) > n - i - 1) q = 0
                    if (q != currentStates(i)) {
                        futureStates = futureStates.updated(i, q)
                        if (j < m && is(j) == i) j += 1
                    }
                    i += 1
                } while (i < n && q > 0 && (i - 1 > currentFailurePosition || futureStates(i - 1) != currentStates(i - 1)))
            } while (i < n && q > 0 && j < m)
            if (q == 0 || i == n) {
                // We either failed or reached some state q at the end of the sequence.
                i -= 1
                futureCosts = computeCosts(futureStates, i, q)
                futureFailurePosition = if (q == 0) i else n
            } else {
                // We found a fixed point i < currentFailurePosition, so nothing changes.
            }
        }
        assert(futureCosts >= 0)
        effect.a = BooleanValue.get(futureCosts)
        effects
    }

    override def commit(before: SearchState, after: SearchState, move: Move) = {
        currentStates = futureStates
        currentCosts = futureCosts
        currentFailurePosition = futureFailurePosition
        effects
    }

    // Cost model: see the class comment
    @inline def computeCosts(states: immutable.IndexedSeq[Int], i: Int, q: Int): Int = {
        assert(i >= 0 && i < n)
        assert(q == 0 || i == n - 1)
        // We avoid the search by relying on distance checking in initialize and consult.
        if (q == 0) n - i else 0
    }

}
