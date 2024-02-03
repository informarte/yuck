package yuck.constraints

import org.jgrapht.alg.matching.HopcroftKarpMaximumCardinalityBipartiteMatching
import org.jgrapht.graph.DefaultUndirectedGraph

import scala.collection.*
import scala.jdk.CollectionConverters.*

import yuck.core.{given, *}
import yuck.util.logging.LazyLogger

/**
 * Implements the ''all_different_int'' constraint as specified by MiniZinc.
 *
 * Given a set X of variables, the constraint maintains the set A = {s(x): x in X} of values
 * assigned to the variables and provides |X| - |A| as measure of constraint violation.
 *
 * @see [[yuck.Notation Notation]]
 *
 * @author Michael Marte
 */
final class Alldistinct
    [V <: Value[V]]
    (id: Id[Constraint], override val maybeGoal: Option[Goal],
     xs: immutable.IndexedSeq[Variable[V]], costs: BooleanVariable,
     logger: LazyLogger)
    (using valueTraits: ValueTraits[V])
    extends ValueFrequencyTracker[V, BooleanValue](id, xs, costs)
{

    override def toString = "alldistinct([%s], %s)".format(xs.mkString(", "), costs)

    override def propagate() = {
        if (valueTraits == IntegerSetValueTraits) {
            // bail out because integer-set domains do not support the diff operation
            NoPropagationOccurred
        } else if (costs.domain == TrueDomain) {
            NoPropagationOccurred.pruneDomains(
                for (x <- xs.iterator if x.domain.isSingleton;
                     y <- xs.iterator if y != x && y.domain.contains(x.domain.singleValue))
                yield (y, y.domain.diff(x.domain))
            )
        } else {
            NoPropagationOccurred
        }
    }

    override protected def computeResult(searchState: SearchState, valueRegistry: ValueRegistry) =
        BooleanValue(xs.size - valueRegistry.size)

    override def isCandidateForImplicitSolving(space: Space) = {
        val (xs, ys) = this.xs.partition(! _.domain.isSingleton)
        val as = ys.iterator.map(_.domain.singleValue).toSet
        valueTraits == IntegerValueTraits &&
        xs.size > 1 &&
        xs.toSet.size == xs.size &&
        ! xs.exists(space.isChannelVariable) &&
        xs.forall(_.domain.isFinite) &&
        xs.forall(x => ! as.exists(a => x.domain.contains(a))) &&
        ys.size == as.size
    }

    override def createNeighbourhood(
        space: Space,
        randomGenerator: RandomGenerator,
        moveSizeDistribution: Distribution,
        createHotSpotDistribution: Seq[AnyVariable] => Option[Distribution],
        maybeFairVariableChoiceRate: Option[Probability]):
        Option[Neighbourhood] =
    {
        if (isCandidateForImplicitSolving(space)) {
            abstract class Vertex
            case class VariableVertex(x: Variable[V]) extends Vertex
            case class ValueVertex(a: V) extends Vertex
            case class Edge(x: Variable[V], a: V)
            val graph = new DefaultUndirectedGraph[Vertex, Edge](classOf[Edge])
            val as = xs.foldLeft(valueTraits.emptyDomain){case (u, x) => u.union(x.domain)}.values
            val variableVertices = xs.iterator.map(x => (x, VariableVertex(x))).toMap
            val valueVertices = as.iterator.map(a => (a, ValueVertex(a))).toMap
            logger.withTimedLogScope("Building graph") {
                for (v <- variableVertices.values) {
                    graph.addVertex(v)
                }
                for (v <- valueVertices.values) {
                    graph.addVertex(v)
                }
                for (x <- xs) {
                    for (a <- x.domain.values) {
                        graph.addEdge(variableVertices(x), valueVertices(a), Edge(x, a))
                    }
                }
                logger.log("Added %d nodes and %d edges".format(graph.vertexSet.size, graph.edgeSet.size))
            }
            val (matching, _) = logger.withTimedLogScope("Computing matching") {
                val matchingAlgo = new HopcroftKarpMaximumCardinalityBipartiteMatching(
                    graph,
                    variableVertices.values.toSet.asJava,
                    valueVertices.values.toSet.asJava)
                matchingAlgo.getMatching
            }
            if (matching.getEdges.size < xs.size) {
                logger.log("Unsatisfiable")
                None
            } else {
                for (Edge(x, a) <- matching.getEdges.asScala) {
                    space.setValue(x, a)
                }
                space.setValue(costs, True)
                Some(new AlldistinctNeighbourhood(space, xs.filter(! _.domain.isSingleton), randomGenerator, moveSizeDistribution))
            }
        } else {
            None
        }
    }

}
