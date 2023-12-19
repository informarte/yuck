package yuck.constraints

import scala.annotation.tailrec
import scala.collection.*

import yuck.core.*
import yuck.util.arm.Sigint
import yuck.util.logging.LazyLogger

/**
 * Implements the ''circuit'' constraint as specified by MiniZinc.
 *
 * Given a sequence (succ_i), offset <= i < offset + n, of variables, this constraint maintains the graph
 * {(i, s(succ_i)): offset <= i < offset + n} (and the cycles in this graph) in order to provide
 * n - (length of the longest cycle) as measure of constraint violation.
 *
 * @see [[yuck.Notation Notation]]
 *
 * @author Michael Marte
 */
final class Circuit
    (id: Id[Constraint], override val maybeGoal: Option[Goal],
     succ: immutable.IndexedSeq[IntegerVariable], offset: Int, costs: BooleanVariable)
    extends CircuitTracker(id, succ, offset, costs)
{

    override def toString = "circuit([%s], %d, %s)".format(succ.mkString(", "), offset, costs)

    override protected def computeCosts(cycleLengths: Iterable[Int]) =
        BooleanValue(succ.size - (if (cycleLengths.isEmpty) 0 else cycleLengths.max))

    override def isCandidateForImplicitSolving(space: Space) =
        succ.size > 2 &&
        succ.size == succ.toSet.size &&
        succ.forall(! space.isChannelVariable(_)) &&
        succ.forall(_.domain.isFinite)

    override def createNeighbourhood(
        space: Space,
        randomGenerator: RandomGenerator,
        moveSizeDistribution: Distribution,
        logger: LazyLogger,
        sigint: Sigint,
        extraCfg: ExtraNeighbourhoodFactoryConfiguration):
        Option[Neighbourhood] =
    {
        if (isCandidateForImplicitSolving(space) && extraCfg.maxNumberOfGreedyHeuristicRuns > 0) {
            solve(
                extraCfg.maxNumberOfGreedyHeuristicRuns - 1,
                logger.withTimedLogScope("Trying deterministic greedy heuristic") {
                    greedyHeuristic(space, randomGenerator, FirstFailStrategy, logger)
                },
                () => logger.withTimedLogScope("Trying randomized greedy heuristic") {
                    greedyHeuristic(space, randomGenerator, RandomizedStrategy, logger)
                },
                logger,
                sigint
            )
        } else {
            None
        }
    }

    private trait GreedyStrategy
    private case object FirstFailStrategy extends GreedyStrategy
    private case object RandomizedStrategy extends GreedyStrategy

    private trait HeuristicResult
    private case object UnsatisfiableInstance extends HeuristicResult
    private case object HeuristicFailed extends HeuristicResult
    private case class HeuristicSucceeded(neighbourhood: Neighbourhood) extends HeuristicResult

    @tailrec
    private def solve
        (n: Int, result: HeuristicResult, heuristic: () => HeuristicResult, logger: LazyLogger, sigint: Sigint):
        Option[Neighbourhood] =
        if (n == 0) None
        else result match {
            case UnsatisfiableInstance => None
            case HeuristicFailed =>
                if (sigint.isSet) {
                    logger.log("Interrupted")
                    None
                } else solve(n - 1, heuristic(), heuristic, logger, sigint)
            case HeuristicSucceeded(neighbourhood) => Some(neighbourhood)
        }

    private def greedyHeuristic(
        space: Space, randomGenerator: RandomGenerator, strategy: GreedyStrategy, logger: LazyLogger):
        HeuristicResult =
    {
        val n = succ.size
        val nodeRange = Range(0, n)

        val maybeSuccessor: Array[Option[Int]] = Array.fill(n)(None)
        def nodeIsSettled(i: Int) = maybeSuccessor(i).isDefined
        val maybePredecessor: Array[Option[Int]] = Array.fill(n)(None)
        def nodeIsInUse(i: Int) = maybePredecessor(i).isDefined
        val numRefs = Array.fill(n)(0)
        // start(i) is the start node of a chain when i is the end node of this chain.
        // (When i is part of a cycle, start(i) does not make sense any more.)
        val start = Array.tabulate(n)(identity)
        // end(i) is the end node of a chain when i is the start node of this chain.
        // (When i is part of a cycle, end(i) does not make sense any more.)
        val end = Array.tabulate(n)(identity)
        // length(i) is the length of the chain ending in node i.
        // (When i is part of a cycle, length(i) does not make sense any more.)
        val length = Array.fill(n)(1)
        var numSettledNodes = 0

        var failed = false
        var unsatisfiable = false

        for (i <- 0 until n if ! failed && succ(i).domain.isSingleton) {
            val j = succ(i).domain.singleValue.toInt - offset
            if (maybePredecessor(j).isDefined) {
                logger.log("Detected a node with two incoming arcs")
                failed = true
                unsatisfiable = true
            } else if (start(i) == j && length(i) < n - 1) {
                logger.log("Detected a subcircuit")
                failed = true
                unsatisfiable = true
            } else {
                maybePredecessor(j) = Some(i)
                maybeSuccessor(i) = Some(j)
                numRefs(j) += 1
                start(end(j)) = start(i)
                end(start(i)) = end(j)
                length(end(j)) += length(i)
                numSettledNodes += 1
            }
        }

        while (numSettledNodes < n && ! failed) {
            val i = strategy match {
                case FirstFailStrategy =>
                    nodeRange.iterator.filterNot(nodeIsSettled).minBy(i => succ(i).domain.size)
                case RandomizedStrategy =>
                    val openNodes = nodeRange.filterNot(nodeIsSettled)
                    openNodes(randomGenerator.nextInt(openNodes.size))
            }
            def isCandidate(j: Int) =
                j != i && ! nodeIsInUse(j) && succ(i).domain.contains(IntegerValue(j + offset)) &&
                    (numSettledNodes == n - 1 || end(j) != i)
            val maybeJ = strategy match {
                case FirstFailStrategy =>
                    nodeRange.iterator.filter(isCandidate).minByOption(numRefs)
                case RandomizedStrategy =>
                    val candidates = nodeRange.filter(isCandidate)
                    if (candidates.isEmpty) None
                    else Some(candidates(randomGenerator.nextInt(candidates.size)))
            }
            if (maybeJ.isDefined) {
                val j = maybeJ.get
                maybePredecessor(j) = Some(i)
                maybeSuccessor(i) = Some(j)
                start(end(j)) = start(i)
                end(start(i)) = end(j)
                length(end(j)) += length(i)
                numSettledNodes += 1
            } else {
                failed = true
            }
        }

        if (unsatisfiable) {
            logger.log("Unsatisfiable")
            UnsatisfiableInstance
        } else if (failed) {
            logger.log("Failed")
            HeuristicFailed
        } else {
            logger.log("Success")
            for (i <- nodeRange) {
                val x = succ(i)
                val a = IntegerValue(maybeSuccessor(i).get + offset)
                assert(x.domain.contains(a))
                space.setValue(x, a)
            }
            space.setValue(costs, True)
            HeuristicSucceeded(new CircuitNeighbourhood(space, succ, offset, randomGenerator))
        }

    }

}

/**
 * Companion object to Circuit.
 *
 * @author Michael Marte
 */
object Circuit {

    def isHamiltonianCircuit(succ: IndexedSeq[IntegerVariable], offset: Int, searchState: SearchState): Boolean = {
        val cycles = CircuitTracker.findCycles(succ, offset, searchState)
        cycles.size == 1 && cycles.get(0).size == succ.size
    }

}
