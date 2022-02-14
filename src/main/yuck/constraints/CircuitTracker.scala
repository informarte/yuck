package yuck.constraints

import org.jgrapht.alg.cycle.TarjanSimpleCycles
import org.jgrapht.graph.{DefaultDirectedGraph, DefaultEdge}

import scala.collection.*
import scala.jdk.CollectionConverters.*

import yuck.core.*

/**
 * Base class for constraints that need to keep track of cycles.
 *
 * @author Michael Marte
 */
abstract class CircuitTracker
    (id: Id[Constraint],
     succ: immutable.IndexedSeq[IntegerVariable], offset: Int, costs: BooleanVariable)
    extends Constraint(id)
{

    final override def inVariables = succ
    final override def outVariables = List(costs)

    private val effect = costs.reuseableEffect

    private def computeCycleLengths(searchState: SearchState): Seq[Int] = {

        val cycleLengths = new mutable.ArrayBuffer[Int]

        val pending = new mutable.HashSet[IntegerVariable]
        val path = new mutable.AnyRefMap[IntegerVariable, Int /* 0-based position on path */]

        pending ++= succ
        var x: IntegerVariable = null
        while (! pending.isEmpty || ! path.isEmpty) {
            if (path.isEmpty) {
                x = pending.head
                pending -= x
                path += x -> 0
            }
            val j = searchState.value(x).value - offset
            if (j < 0 || j >= succ.size) {
                // handle invalid node reference
                path.clear()
            } else {
                val y = succ(j)
                if (path.contains(y)) {
                    // we found a cycle
                    cycleLengths += path.size - path(y)
                    path.clear()
                    pending -= y
                } else if (pending.contains(y)) {
                    // y has not yet been visited, so extend path with y
                    path += y -> path.size
                    pending -= y
                    x = y
                } else {
                    // abandon path because y is part of an already known cycle or on an
                    // already walked path leading to such a cycle
                    path.clear()
                }
            }
        }

        cycleLengths
    }

    override def propagate = {
        if (costs.domain == TrueDomain) {
            val indexRange = IntegerRange(offset, offset + succ.size - 1)
            NoPropagationOccurred
                .pruneDomains(
                    for (i <- 0 until succ.size) yield {
                        val a = IntegerValue(offset + i)
                        (succ(i), succ(i).domain.intersect(indexRange).diff(IntegerRange(a, a)))
                    }
                )
                .pruneDomains(
                    for (x <- succ.iterator if x.domain.isSingleton;
                         y <- succ.iterator if y != x && y.domain.contains(x.domain.singleValue))
                        yield (y, y.domain.diff(x.domain))
                )
        } else {
            NoPropagationOccurred
        }
    }

    final override def initialize(now: SearchState) = {
        val cycleLengths = computeCycleLengths(now)
        effect.a = computeCosts(cycleLengths)
        effect
    }

    final override def consult(before: SearchState, after: SearchState, move: Move) =
        initialize(after)

    final override def commit(before: SearchState, after: SearchState, move: Move) =
        effect

    /** Subclasses are required to define this method for computing the costs from the given cycles. */
    protected def computeCosts(cycleLengths: Iterable[Int]): BooleanValue

}

/**
 * Companion object to CircuitTracker.
 *
 * @author Michael Marte
 */
object CircuitTracker {

    private type Graph = DefaultDirectedGraph[IntegerVariable, DefaultEdge]

    def findCycles
        (succ: IndexedSeq[IntegerVariable], offset: Int, searchState: SearchState):
        java.util.List[java.util.List[IntegerVariable]] =
    {
        val graph = new Graph(classOf[DefaultEdge])
        for (x <- succ) {
            graph.addVertex(x)
        }
        for (x <- succ) {
            val i = searchState.value(x).value - offset
            if (i >= 0 && i < succ.size) {
                graph.addEdge(x, succ(i))
            }
        }
        val cycles = new TarjanSimpleCycles(graph).findSimpleCycles
        cycles
    }

}