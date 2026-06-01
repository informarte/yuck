package yuck.constraints

import scala.collection.*
import scala.jdk.CollectionConverters.*

import org.jgrapht.alg.matching.{HopcroftKarpMaximumCardinalityBipartiteMatching, MaximumWeightBipartiteMatching}
import org.jgrapht.graph.{DefaultUndirectedGraph, DefaultUndirectedWeightedGraph}

import yuck.core.*
import yuck.util.arm.Sigint
import yuck.util.logging.LazyLogger

/**
 * Can be used to implement constraints like ''all_different'' and ''all_different_except_0''.
 *
 * Given a set X of variables and a set of values S, the constraint maintains the set A = {s(x): x in X}
 * of values assigned to the variables and provides |{x in X: s(x) not in S}| - |A \ S| as measure of
 * constraint violation.
 *
 * @see [[yuck.Notation Notation]]
 */
final class AllDifferent
    [A <: Value[A], D <: Domain[A, D], X <: Variable[A, D, X]]
    (id: Id[Constraint],
     override val xs: immutable.IndexedSeq[X],
     val exceptedValues: immutable.Set[A],
     override val result: BooleanVariable)
    (using override protected val typeTraits: TypeTraits[A, D, X])
    extends ValueFrequencyTracker[A, D, X, BooleanValue, BooleanDomain, BooleanVariable](id)
{

    override def toString =
        if exceptedValues.isEmpty
        then "all_different([%s], %s)".format(xs.mkString(", "), result)
        else "all_different_except([%s], {%s}, %s)".format(xs.mkString(", "), exceptedValues.mkString(", "), result)

    override def copy(replacements: Map[AnyVariable, AnyVariable]) =
        new AllDifferent(id, xs, exceptedValues, replacements.getOrElse(result, result).asInstanceOf[BooleanVariable])

    override def propagate() = {
        if result.domain == TrueDomain && typeTraits.domainCapabilities.diff then {
            NoPropagationOccurred.pruneDomains(
                for x <- xs.iterator if x.domain.isSingleton && ! exceptedValues.contains(x.domain.singleValue)
                    y <- xs.iterator if y != x && y.domain.contains(x.domain.singleValue)
                yield
                    (y, y.domain.diff(x.domain))
            )
        } else {
            NoPropagationOccurred
        }
    }

    final override protected def computeResult(searchState: SearchState, valueRegistry: ValueRegistry) = {
        var violation = xs.size - valueRegistry.size
        for a <- exceptedValues do {
            val maybeCount = valueRegistry.get(a)
            if maybeCount.isDefined then {
                violation -= maybeCount.get - 1
            }
        }
        BooleanValue(violation)
    }

    override def isCandidateForImplicitSolving(space: Space) = {
        val (ys, xs) = this.xs.partition(_.domain.isSingleton)
        val as = ys.iterator.map(_.domain.singleValue).toSet
        typeTraits.domainCapabilities.createDomain &&
            typeTraits.domainCapabilities.diff &&
            typeTraits.domainCapabilities.union &&
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
        logger: LazyLogger,
        sigint: Sigint,
        moveSizeDistribution: Distribution,
        createHotSpotDistribution: IndexedSeq[AnyVariable] => Option[Distribution],
        maybeFairVariableChoiceRate: Option[Probability]):
        Option[Neighbourhood] =
    {
        if isCandidateForImplicitSolving(space) then {
            abstract class Vertex
            case class VariableVertex(x: X) extends Vertex
            case class ValueVertex(a: A) extends Vertex
            case class ExceptedValueVertex(x: X, a: A) extends Vertex
            case class Edge(x: X, a: A)
            val graph =
                if exceptedValues.isEmpty
                then new DefaultUndirectedGraph[Vertex, Edge](classOf[Edge])
                else new DefaultUndirectedWeightedGraph[Vertex, Edge](classOf[Edge])
            val as = xs.foldLeft(typeTraits.emptyDomain)((u, x) => u.union(x.domain)).values
            val variableVertices = xs.iterator.map(x => (x, VariableVertex(x))).toMap
            val valueVertices = as.iterator.filterNot(exceptedValues.contains).map(a => (a, ValueVertex(a))).toMap
            val exceptedValueVertices = new mutable.ArrayBuffer[Vertex]
            logger.withTimedLogScope("Building graph") {
                for v <- variableVertices.values do {
                    graph.addVertex(v)
                }
                for v <- valueVertices.values do {
                    graph.addVertex(v)
                }
                for x <- xs do {
                    for a <- x.domain.values do {
                        val e = Edge(x, a)
                        if exceptedValues.isEmpty then {
                            graph.addEdge(variableVertices(x), valueVertices(a), e)
                        } else if exceptedValues.contains(a) then {
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
            if matching.getEdges.size < xs.size then {
                logger.log("Unsatisfiable")
                None
            } else {
                for Edge(x, a) <- matching.getEdges.asScala do {
                    space.setValue(x, a)
                }
                space.setValue(result, True)
                val ys = xs.filterNot(_.domain.isSingleton)
                Some(new AllDifferentNeighbourhood(
                    space, ys, exceptedValues,
                    randomGenerator,
                    moveSizeDistribution, createHotSpotDistribution(ys), maybeFairVariableChoiceRate))
            }
        } else {
            None
        }
    }

}
