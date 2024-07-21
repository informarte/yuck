package yuck.constraints

import org.jgrapht.alg.matching.HopcroftKarpMaximumCardinalityBipartiteMatching
import org.jgrapht.alg.matching.MaximumWeightBipartiteMatching
import org.jgrapht.graph.DefaultUndirectedGraph
import org.jgrapht.graph.DefaultUndirectedWeightedGraph

import scala.collection.*
import scala.jdk.CollectionConverters.*

import yuck.core.*
import yuck.util.logging.LazyLogger

/**
 * Can be used to implement constraints like ''all_different'' and ''all_different_except_0''.
 *
 * Given a set X of variables and a set of values S, the constraint maintains the set A = {s(x): x in X}
 * of values assigned to the variables and provides |{x in X: s(x) not in S}| - |A \ S| as measure of
 * constraint violation.
 *
 * @see [[yuck.Notation Notation]]
 *
 * @author Michael Marte
 */
final class AllDifferent
    [V <: Value[V]]
    (id: Id[Constraint],
     override val maybeGoal: Option[Goal],
     override protected val xs: immutable.IndexedSeq[Variable[V]],
     exceptedValues: immutable.Set[V],
     override protected val result: BooleanVariable,
     logger: LazyLogger)
    (using override protected val valueTraits: ValueTraits[V])
    extends ValueFrequencyTracker[V, BooleanValue](id)
{

    override def toString =
        if exceptedValues.isEmpty
        then "all_different([%s], %s)".format(xs.mkString(", "), result)
        else "all_different_except([%s], {%s}, %s)".format(xs.mkString(", "), exceptedValues.mkString(", "), result)

    override def propagate() = {
        if (result.domain == TrueDomain && valueTraits.domainCapabilities.diff) {
            NoPropagationOccurred.pruneDomains(
                for (x <- xs.iterator if x.domain.isSingleton && ! exceptedValues.contains(x.domain.singleValue);
                     y <- xs.iterator if y != x && y.domain.contains(x.domain.singleValue))
                yield (y, y.domain.diff(x.domain))
            )
        } else {
            NoPropagationOccurred
        }
    }

    final override protected def computeResult(searchState: SearchState, valueRegistry: ValueRegistry) = {
        var violation = xs.size - valueRegistry.size
        for (a <- exceptedValues) {
            val maybeCount = valueRegistry.get(a)
            if (maybeCount.isDefined) {
                violation -= maybeCount.get - 1
            }
        }
        BooleanValue(violation)
    }

    override def isCandidateForImplicitSolving(space: Space) = {
        val (ys, xs) = this.xs.partition(_.domain.isSingleton)
        val as = ys.iterator.map(_.domain.singleValue).toSet
        valueTraits.domainCapabilities.createDomain &&
            valueTraits.domainCapabilities.diff &&
            valueTraits.domainCapabilities.union &&
            xs.size > 1 &&
            xs.toSet.size == xs.size &&
            ! xs.exists(space.isChannelVariable) &&
            xs.forall(_.domain.isFinite) &&
            xs.forall(x => ! as.exists(x.domain.contains)) &&
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
            case class ExceptedValueVertex(x: Variable[V], a: V) extends Vertex
            case class Edge(x: Variable[V], a: V)
            val graph =
                if exceptedValues.isEmpty
                then new DefaultUndirectedGraph[Vertex, Edge](classOf[Edge])
                else new DefaultUndirectedWeightedGraph[Vertex, Edge](classOf[Edge])
            val as = xs.foldLeft(valueTraits.emptyDomain)((u, x) => u.union(x.domain)).values
            val variableVertices = xs.iterator.map(x => (x, VariableVertex(x))).toMap
            val valueVertices = as.iterator.filterNot(exceptedValues.contains).map(a => (a, ValueVertex(a))).toMap
            val exceptedValueVertices = new mutable.ArrayBuffer[Vertex]
            logger.withTimedLogScope("Building graph") {
                for (v <- variableVertices.values) {
                    graph.addVertex(v)
                }
                for (v <- valueVertices.values) {
                    graph.addVertex(v)
                }
                for (x <- xs) {
                    for (a <- x.domain.values) {
                        val e = Edge(x, a)
                        if (exceptedValues.isEmpty) {
                            graph.addEdge(variableVertices(x), valueVertices(a), e)
                        } else if (exceptedValues.contains(a)) {
                            val v = ExceptedValueVertex(x, a)
                            graph.addVertex(v)
                            graph.addEdge(variableVertices(x), v, e)
                            graph.setEdgeWeight(e, 1)
                            exceptedValueVertices += v
                        } else {
                            graph.addEdge(variableVertices(x), valueVertices(a), e)
                            graph.setEdgeWeight(e, 2)
                        }
                    }
                }
                logger.log("Added %d nodes and %d edges".format(graph.vertexSet.size, graph.edgeSet.size))
            }
            val (matching, _) = logger.withTimedLogScope("Computing matching") {
                // MaximumWeightBipartiteMatching requires positive edge weights!
                val matchingAlgo =
                    if exceptedValues.isEmpty
                    then new HopcroftKarpMaximumCardinalityBipartiteMatching[Vertex, Edge](
                        graph,
                        variableVertices.values.toSet.asJava,
                        valueVertices.values.toSet.asJava)
                    else new MaximumWeightBipartiteMatching[Vertex, Edge](
                        graph,
                        variableVertices.values.toSet.asJava,
                        valueVertices.values.concat(exceptedValueVertices).toSet.asJava)
                matchingAlgo.getMatching
            }
            if (matching.getEdges.size < xs.size) {
                logger.log("Unsatisfiable")
                None
            } else {
                for (Edge(x, a) <- matching.getEdges.asScala) {
                    space.setValue(x, a)
                }
                space.setValue(result, True)
                Some(new AllDifferentNeighbourhood(space, xs.filterNot(_.domain.isSingleton), exceptedValues, randomGenerator, moveSizeDistribution))
            }
        } else {
            None
        }
    }

}
