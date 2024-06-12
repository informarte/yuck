package yuck.constraints

import scala.collection.*

import yuck.core.*
import yuck.util.logging.LazyLogger

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
 * @author Michael Marte
 */
final class Regular
    (id: Id[Constraint], override val maybeGoal: Option[Goal],
     dfa: RegularDfa,
     costs: BooleanVariable,
     logger: LazyLogger)
    extends Constraint(id)
{

    import RegularGraph.*

    inline private def xs = dfa.xs
    inline private def Q = dfa.Q
    inline private def S = dfa.S
    inline private def delta = dfa.delta
    inline private def q0 = dfa.q0
    inline private def F = dfa.F

    private val n = xs.size
    private val hasDuplicateVariables = xs.toSet.size < n

    private val (distancesToAcceptingState: Vector[Int], _) = logger.withTimedLogScope("Computing distances") {
        // We use the Floyd-Warshall algorithm to compute, for each q in Q, the minimum number of
        // transitions that are required to reach an accepting state from q.
        val d = Array.ofDim[Int](Q, Q)
        for (u <- 0 until Q)
            for (v <- 0 until Q)
                d(u)(v) = if (u == v) 0 else Int.MaxValue
        for (u <- 0 until Q)
            for (a <- 0 until S) {
                val v = delta(u)(a) - 1
                if (v > -1 /* ignore failed state */ && u != v) d(u)(v) = 1
            }
        for (w <- 0 until Q)
            for (u <- 0 until Q)
                for (v <- 0 until Q)
                    if (d(u)(w) < Int.MaxValue && d(w)(v) < Int.MaxValue) {
                        val duwv = d(u)(w) + d(w)(v)
                        if (d(u)(v) > duwv) d(u)(v) = duwv
                    }
        Vector.tabulate(Q)(i => F.valuesIterator.map(f => d(i)(f.toInt - 1)).min)
    }

    override def toString =
        "regular([%s], %d, %d, [%s], %d, %s, %s)".format(
            xs.mkString(", "), Q, S,
            delta.iterator.map(row => "[%s]".format(row.mkString(", "))).mkString(", "), q0, F, costs)

    override def inVariables = xs
    override def outVariables = List(costs)

    private val x2i: HashMap[AnyVariable, Int] =
        if hasDuplicateVariables then null else xs.view.zipWithIndex.to(HashMap)
    private val x2is: HashMap[AnyVariable, Vector[Int]] =
        if hasDuplicateVariables
        then xs.view.zipWithIndex.groupBy(_._1).view.mapValues(_.map(_._2).toVector).to(HashMap)
        else null

    private var currentCosts = 0
    private var currentStates: Vector[Int] = null
    // The position at which the current sequence is recognized as invalid.
    // All states after this position are to be considered as failed!
    // (We do not maintain those states i.e. they may actually be greater than 0.)
    private var currentFailurePosition = 0
    private var futureCosts = 0
    private var futureStates: Vector[Int] = null
    private var futureFailurePosition = 0
    private val effect = costs.reuseableEffect

    override def initialize(now: SearchState) = {
        currentStates = Vector.fill(n)(0)
        var i = 0
        var q = q0
        while (i < n && q > 0) {
            val a = now.value(xs(i)).toInt
            q = if (a >= 1 && a <= S) delta(q - 1)(a - 1) else 0
            if (q > 0 && distancesToAcceptingState(q - 1) > n - i - 1) q = 0
            currentStates = currentStates.updated(i, q)
            i += 1
        }
        i -= 1
        currentCosts = computeCosts(i, q)
        currentFailurePosition = if (q == 0) i else n
        assert(currentCosts >= 0)
        effect.a = BooleanValue(currentCosts)
        effect
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
            val xs = move.involvedVariablesIterator
            val is0 = if (hasDuplicateVariables) xs.flatMap(x2is) else xs.map(x2i)
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
            while {
                i = is(j)
                j += 1
                q = if (i == 0) q0 else futureStates(i - 1)
                while {
                    val a = after.value(xs(i)).toInt
                    q = if (a >= 1 && a <= S) delta(q - 1)(a - 1) else 0
                    if (q > 0 && distancesToAcceptingState(q - 1) > n - i - 1) q = 0
                    if (q != currentStates(i)) {
                        futureStates = futureStates.updated(i, q)
                        if (j < m && is(j) == i) j += 1
                    }
                    i += 1
                    i < n && q > 0 && (i - 1 > currentFailurePosition || futureStates(i - 1) != currentStates(i - 1))
                } do ()
                i < n && q > 0 && j < m
            } do ()
            if (q == 0 || i == n) {
                // We either failed or reached some state q at the end of the sequence.
                i -= 1
                futureCosts = computeCosts(i, q)
                futureFailurePosition = if (q == 0) i else n
            } else {
                // We found a fixed point i < currentFailurePosition, so nothing changes.
            }
        }
        assert(futureCosts >= 0)
        effect.a = BooleanValue(futureCosts)
        effect
    }

    override def commit(before: SearchState, after: SearchState, move: Move) = {
        currentStates = futureStates
        currentCosts = futureCosts
        currentFailurePosition = futureFailurePosition
        effect
    }

    // Cost model: see the class comment
    private def computeCosts(i: Int, q: Int): Int = {
        assert(i >= 0 && i < n)
        assert(q == 0 || i == n - 1)
        // We avoid the search by relying on distance checking in initialize and consult.
        if (q == 0) n - i else 0
    }

    override def isCandidateForImplicitSolving(space: Space) =
        xs.exists(space.isSearchVariable) &&
        ! xs.exists(space.isChannelVariable) &&
        xs.forall(_.domain.isFinite) &&
        ! hasDuplicateVariables

    override def createNeighbourhood(
        space: Space,
        randomGenerator: RandomGenerator,
        moveSizeDistribution: Distribution,
        createHotSpotDistribution: Seq[AnyVariable] => Option[Distribution],
        maybeFairVariableChoiceRate: Option[Probability]) =
    {
        if (isCandidateForImplicitSolving(space)) {
            val (graph, _) = logger.withTimedLogScope("Building graph") {
                val graph = new RegularGraph(dfa)
                logger.log(String.format("Graph has %d nodes and %d edges", graph.numberOfNodes, graph.numberOfEdges))
                graph
            }
            graph.computeShortestPath(_ => 0).map(path =>
                assert(path.length == n + 1) // source is excluded, sink is included
                for (case Assignment(_, x, d, _) <- path) {
                    space.setValue(x, d.randomValue(randomGenerator))
                }
                space.setValue(costs, True)
                val initialPath = path.toVector
                new RegularNeighbourhood(
                    space, xs, randomGenerator,
                    moveSizeDistribution, createHotSpotDistribution(xs), maybeFairVariableChoiceRate,
                    graph, initialPath)
            )
        } else {
            None
        }
    }

}
