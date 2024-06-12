package yuck.constraints

import org.jgrapht.Graph
import org.jgrapht.graph.DirectedMultigraph
import org.jgrapht.traverse.TopologicalOrderIterator

import scala.collection.*
import scala.jdk.CollectionConverters.*
import scala.reflect.ClassTag

import yuck.core.*

/**
 * RegularGraph is used for solving ''regular'' constraints without duplicate variables.
 *
 * RegularGraph is a DAG with initial state q0 as source, one layer for each input variable, a sink
 * (representing acceptance), and the following edges:
 *
 *  - For each a in the domain of xs[0], the source is connected to (xs[0], u) by an edge (q0, xs[0], a, u)
 *    if delta(q0, a) = u and u != 0.
 *  - For each 0 < i < n, a in the domain of xs[i], and state (xs[i - 1], u), the latter is connected to
 *    (xs[i], v) by the edge (u, xs[i], a, v) if delta(u, a) = v and v != 0.
 *  - Each state (xs[n - 1], u) is connected to the sink if u is in F (is an accepting state).
 *
 * Therefore, each path from the source to the sink is a solution to the ''regular'' constraint.
 *
 * This graph has at most 2 + |Q||xs| nodes and 2|Q| + (|xs| - 1)|Q||S| edges.
 *
 * The implementation uses two representations:
 *
 *  - A map-based graph from the JGraphT library
 *  - An array-based graph (derived from the map-based graph) for faster shortest-path search
 *
 * This class is tested by RegularTest and RegularNeighbourhoodTest.
 *
 * @author Michael Marte
 */
final class RegularGraph(dfa: RegularDfa) {

    inline private def xs = dfa.xs

    private val n = xs.size

    require(xs.toSet.size == n)

    import RegularGraph.{*, given}

    private val source = new State {
        override val q = dfa.q0
        override def toString = "source"
    }

    private val sink = new State {
        override val q = dfa.Q
        override def toString = "sink"
    }

    private case class AcceptingStateToSink(override val q: Int) extends Transition {
        override def toString = "%s -> sink".format(q)
        override val u = dfa.Q
    }

    private val mapBasedRepresentation: Graph[State, Transition] = {

        val graph = new DirectedMultigraph[State, Transition](classOf[Transition])

        // add source and layers
        graph.addVertex(source)
        val lastLayer = xs.foldLeft(Set(source))((previousLayer, x) =>
            val nextLayer = new mutable.HashSet[State]
            for (state <- previousLayer) {
                x.domain.values
                    .filter(a => a >= One && a.value.toInt <= dfa.S)
                    .groupBy(a => dfa.delta(state.q - 1)(a.value.toInt - 1))
                    .view
                    .filterKeys(_ > 0)
                    .mapValues(IntegerDomain.apply)
                    .mapValues(d => if d.isSubsetOf(SixtyFourBitSet.ValueRange) then SixtyFourBitSet(d) else d)
                    .foreach((nextQ, d) => {
                        val nextState = IntermediateState(x, nextQ)
                        graph.addVertex(nextState)
                        val edge = Assignment(state.q, x, d, nextState.q)
                        graph.addEdge(state, nextState, edge)
                        nextLayer += nextState
                    })
            }
            nextLayer
        )

        // add sink and connect it to last layer
        graph.addVertex(sink)
        for (case state @ IntermediateState(_, q) <- lastLayer) {
            if (dfa.F.contains(IntegerValue(q))) {
                graph.addEdge(state, sink, AcceptingStateToSink(q))
            }
        }

        // Backward phase: Starting from the last layer, remove all nodes with no outgoing edges.
        // (The first iteration will remove all non-accepting states from the last layer
        // and subsequent iterations will address the states that were turned into dead ends by
        // earlier iterations.)
        for (x <- xs.reverse) {
            for (q <- dfa.Q to 1 by -1) {
                val state = IntermediateState(x, q)
                if (graph.containsVertex(state) && graph.outgoingEdgesOf(state).isEmpty) {
                    graph.removeVertex(state)
                }
            }
        }

        graph
    }

    val numberOfNodes = mapBasedRepresentation.vertexSet.size

    val numberOfEdges = mapBasedRepresentation.edgeSet.size

    // This array-based representation is significantly faster than JGraphT's map-based representation.
    // The type parameter T allows to save space but this optimization did not yield significant performance
    // improvements on the available test data. Nevertheless it has been kept to demonstrate how C++ like
    // code specialization can be achieved using the inlining feature of Scala 3.
    // To obtain efficient byte code (invokevirtual instead of invokeinterface), final classes are used
    // instead of interfaces/ traits.
    private final class ArrayBasedRepresentation[T](using integral: Integral[T], classTag: ClassTag[T]) {

        private val i2v: Array[State] =
            mapBasedRepresentation.vertexSet.iterator.asScala.toArray
        private val v2i: HashMap[State, Int] =
            (0 until i2v.size).iterator.map(i => (i2v(i), i)).to(HashMap)

        private val i2e: Array[Transition] =
            mapBasedRepresentation.edgeSet.iterator.asScala.toArray
        private val e2i : HashMap[Transition, Int] =
            (0 until i2e.size).iterator.map(i => (i2e(i), i)).to(HashMap)

        private val topologicalOrdering: Array[T] =
            new TopologicalOrderIterator(mapBasedRepresentation).asScala.map(v => integral.fromInt(v2i(v))).toArray[T]

        private val adjacencyLists: Array[Array[T]] =
            Array.tabulate(i2v.size)(i => mapBasedRepresentation.outgoingEdgesOf(i2v(i)).iterator.asScala.map(e => integral.fromInt(e2i(e))).toArray[T])

        private val edgeTargets: Array[T] =
            Array.tabulate(i2e.size)(i => integral.fromInt(v2i(mapBasedRepresentation.getEdgeTarget(i2e(i)))))

        def computeShortestPath(distanceProvider: DistanceProvider): Option[Array[Transition]] = {
            integral match {
                case ByteAsIntegral => computeShortestPathByte(distanceProvider)
                case ShortAsIntegral => computeShortestPathShort(distanceProvider)
                case IntAsIntegral => computeShortestPathInt(distanceProvider)
            }
        }

        import ArrayBasedRepresentation.*

        private def computeShortestPathByte(distanceProvider: DistanceProvider): Option[Array[Transition]] =
            _computeShortestPath[Byte](this.asInstanceOf[ArrayBasedRepresentation[Byte]], ByteAsIntegral, distanceProvider)

        private def computeShortestPathShort(distanceProvider: DistanceProvider): Option[Array[Transition]] =
            _computeShortestPath[Short](this.asInstanceOf[ArrayBasedRepresentation[Short]], ShortAsIntegral, distanceProvider)

        private def computeShortestPathInt(distanceProvider: DistanceProvider): Option[Array[Transition]] =
            _computeShortestPath[Int](this.asInstanceOf[ArrayBasedRepresentation[Int]], IntAsIntegral, distanceProvider)

    }

    private object ArrayBasedRepresentation {

        // This method implements the shortest-path algorithm for DAGs from Cormen et al.,
        // Introduction to Algorithms, 2nd edition, section 24.2. With its running time in O(V + E),
        // it is more efficient than Dijkstra's algorithm with a Fibonacci heap.
        // (JGraphT does not provide this algorithm.)
        inline private def _computeShortestPath[T]
            (graph: ArrayBasedRepresentation[T], integral: Integral[T], distanceProvider: DistanceProvider)
            (using classTag: ClassTag[T]):
            Option[Array[Transition]] =
        {
            val m = graph.topologicalOrdering.length
            val dist = Array.fill(m)(Int.MaxValue)
            val pred = Array.fill[T](m)(integral.fromInt(-1))
            val edge = Array.fill[T](m)(integral.fromInt(-1)) // edge(i) is the edge leading to vertex i
            var i = 0
            dist(graph.v2i(source)) = 0
            while (i < m) {
                val u = graph.topologicalOrdering(i)
                val outgoingEdges = graph.adjacencyLists(integral.toInt(u))
                val n = outgoingEdges.length
                var j = 0
                while (j < n) {
                    val e = outgoingEdges(j)
                    val v = integral.toInt(graph.edgeTargets(integral.toInt(e)))
                    val d = distanceProvider(graph.i2e(integral.toInt(e)))
                    val td = dist(integral.toInt(u)) + d
                    if (dist(v) > td) {
                        dist(v) = td
                        pred(v) = u
                        edge(v) = e
                    }
                    j += 1
                }
                i += 1
            }
            val path = new Array[Transition](n + 1)
            var v = graph.v2i(sink)
            i = n
            while (i >= 0 && v >= 0) {
                val e = integral.toInt(edge(v))
                path(i) = graph.i2e(e)
                i -= 1
                v = integral.toInt(pred(v))
            }
            if i == -1 then Some(path) else None
        }

    }

    private val arrayBasedRepresentation = {
        val n = max(mapBasedRepresentation.vertexSet.size, mapBasedRepresentation.edgeSet.size)
        if n <= Byte.MaxValue
        then new ArrayBasedRepresentation[Byte]
        else if n <= Short.MaxValue
        then new ArrayBasedRepresentation[Short]
        else new ArrayBasedRepresentation[Int]
    }

    /** Computes a shortest path from the source to the sink. */
    def computeShortestPath(distanceProvider: DistanceProvider): Option[Array[Transition]] =
        arrayBasedRepresentation.computeShortestPath(distanceProvider)

}

object RegularGraph {

    abstract class State {
        val q: Int
    }

    case class IntermediateState(x: IntegerVariable, override val q: Int) extends State

    abstract class Transition {
        val q: Int
        val u: Int
    }

    case class Assignment
        (override val q: Int, x: IntegerVariable, d: IntegerDomain, override val u: Int)
        extends Transition
    {
        override def toString = "%s ->_{%s ∈ %s} %s".format(q, x, d, u)
    }

    abstract class DistanceProvider {
        def apply(transition: Transition): Int
    }

    private abstract class Integral[T] {
        def fromInt(a: Int): T
        def toInt(a: T): Int
    }

    private given ByteAsIntegral: Integral[Byte] with {
        inline override def fromInt(a: Int) = {
            require(a >= Byte.MinValue && a <= Byte.MaxValue)
            a.toByte
        }
        // Calling toInt produces more efficient bytecode.
        inline override def toInt(a: Byte) = a.toInt
    }

    private given ShortAsIntegral: Integral[Short] with {
        inline override def fromInt(a: Int) = {
            require(a >= Short.MinValue && a <= Short.MaxValue)
            a.toShort
        }
        // Calling toInt produces more efficient bytecode.
        inline override def toInt(a: Short) = a.toInt
    }

    private given IntAsIntegral: Integral[Int] with {
        inline override def fromInt(a: Int) = a
        inline override def toInt(a: Int) = a
    }

}
