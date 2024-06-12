package yuck.constraints

import org.jgrapht.alg.matching.HopcroftKarpMaximumCardinalityBipartiteMatching
import org.jgrapht.graph.DefaultUndirectedGraph

import scala.collection.*
import scala.jdk.CollectionConverters.*

import yuck.core.*
import yuck.util.logging.LazyLogger

/**
 * Used in Inverse to represent a function that should be the inverse of another function.
 *
 * @author Michael Marte
 */
final class InverseFunction
    (val xs: immutable.IndexedSeq[IntegerVariable],
     val offset: Int)
{
    require(! xs.isEmpty)
    val indexRange = offset until safeAdd(offset, xs.size)
    val indexDomain = IntegerRange(offset, offset + xs.size - 1)
    val x2i = new HashMap[AnyVariable, Int] ++ xs.zip(indexRange)
    val refs = new Array[mutable.HashSet[Int]](xs.size)
    val visited = new Array[Int](xs.size)
    def isSuitableForImplicitSolving(space: Space) =
        xs.size > 1 &&
        xs.size == xs.toSet.size &&
        xs.forall(! space.isChannelVariable(_)) &&
        xs.forall(_.domain.isFinite)
}

/**
 * Used to implement the ''inverse'' constraint as specified by MiniZinc.
 *
 * Given variables f[fOffset], ..., f[fOffset + n - 1] and g[gOffset], ..., g[gOffset + m - 1],
 * the constraint computes
 *
 *  - |s(g[s(f[i])]) - i| for fOffset <= i < fOffset + n and
 *  - |s(f[s(g[j])]) - j| for gOffset <= j < gOffset + m,
 *
 * and provides the sum of these terms as measure of constraint violation.
 *
 * Out-of-bounds indices are ignored and hence both f and g may contain channel variables.
 * (In this case, additional constraints are required that force the channel variables
 * to take valid values.)
 *
 * @see [[yuck.Notation Notation]]
 *
 * @author Michael Marte
 */
final class Inverse
    (id: Id[Constraint], override val maybeGoal: Option[Goal],
     f: InverseFunction,
     g: InverseFunction,
     costs: BooleanVariable,
     logger: LazyLogger)
    extends Constraint(id)
{

    private val debug = false

    override def toString =
        "inverse([%s], %d, [%s], %d, %s)".format(
            f.xs.mkString(", "), f.offset, g.xs.mkString(", "), g.offset, costs)

    override def inVariables = f.xs.view ++ g.xs.view
    override def outVariables = List(costs)

    private var currentCosts = 0
    private var futureCosts = 0
    private val effect = costs.reuseableEffect

    private def computeCosts(
        f: InverseFunction, g: InverseFunction, i: Int, searchState: SearchState): Int =
    {
        val j = searchState.value(f.xs(i - f.offset)).toInt
        if (g.indexRange.contains(j)) abs(safeSub(searchState.value(g.xs(j - g.offset)).toInt, i))
        else 0
    }

    private def propagate(effects: PropagationEffects, f: InverseFunction, g: InverseFunction): PropagationEffects = {
        effects
            .pruneDomains(for (x <- f.xs.iterator) yield (x, g.indexDomain))
            .pruneDomains(for (x <- g.xs.iterator) yield (x, f.indexDomain))
            .pruneDomains(
                for (i <- f.indexRange.iterator;
                     x = f.xs(i - f.offset);
                     dx = x.domain;
                     di = IntegerRange(i, i);
                     j <- g.indexDomain.diff(dx).valuesIterator.map(_.toInt);
                     y = g.xs(j - g.offset))
                yield
                    (y, y.domain.diff(di)))
            .pruneDomains(
                for (i <- f.indexRange.iterator;
                     x = f.xs(i - f.offset);
                     dx = x.domain;
                     if dx.isSingleton;
                     y = g.xs(dx.singleValue.toInt - g.offset))
                yield
                    (y, y.domain.intersect(IntegerRange(i, i))))
    }

    override def propagate() =
        if (costs.domain == TrueDomain) {
            propagate(propagate(NoPropagationOccurred, f, g), g, f)
        } else {
            NoPropagationOccurred
        }

    override def initialize(now: SearchState) = {
        currentCosts = 0
        for (i <- f.xs.indices) {
            f.visited(i) = -1
            f.refs(i) = new mutable.HashSet[Int]
        }
        for (j <- g.xs.indices) {
            g.visited(j) = -1
            g.refs(j) = new mutable.HashSet[Int]
        }
        for (i <- f.indexRange) {
            currentCosts = safeAdd(currentCosts, computeCosts(f, g, i, now))
            val j = now.value(f.xs(i - f.offset)).toInt
            if (g.indexRange.contains(j)) {
                g.refs(j - g.offset) += i
            }
        }
        for (j <- g.indexRange) {
            currentCosts = safeAdd(currentCosts, computeCosts(g, f, j, now))
            val i = now.value(g.xs(j - g.offset)).toInt
            if (f.indexRange.contains(i)) {
                f.refs(i - f.offset) += j
            }
        }
        assert(currentCosts >= 0)
        effect.a = BooleanValue(currentCosts)
        effect
    }

    override def consult(before: SearchState, after: SearchState, move: Move) = {
        def computeCostDelta(f: InverseFunction, g: InverseFunction, i: Int, visited: Array[Int]): Int = {
            if (! f.indexRange.contains(i) || visited(i - f.offset) == move.id.rawId) {
                0
            } else {
                visited(i - f.offset) = move.id.rawId
                val delta = safeSub(computeCosts(f, g, i, after), computeCosts(f, g, i, before))
                delta
            }
        }
        futureCosts = currentCosts
        for (x <- move) {
            val maybeI = f.x2i.get(x)
            if (maybeI.isDefined) {
                val i = maybeI.get
                val x = f.xs(i - f.offset)
                futureCosts = safeAdd(futureCosts, computeCostDelta(f, g, i, f.visited))
                for (j <- f.refs(i - f.offset)) {
                    futureCosts = safeAdd(futureCosts, computeCostDelta(g, f, j, g.visited))
                }
                futureCosts = safeAdd(futureCosts, computeCostDelta(g, f, after.value(x).toInt, g.visited))
            }
            val maybeJ = g.x2i.get(x)
            if (maybeJ.isDefined) {
                val j = maybeJ.get
                val y = g.xs(j - g.offset)
                futureCosts = safeAdd(futureCosts, computeCostDelta(g, f, j, g.visited))
                for (i <- g.refs(j - g.offset)) {
                    futureCosts = safeAdd(futureCosts, computeCostDelta(f, g, i, f.visited))
                }
                futureCosts = safeAdd(futureCosts, computeCostDelta(f, g, after.value(y).toInt, f.visited))
            }
        }
        if (debug) {
            for (i <- f.visited.indices if f.visited(i) != move.id.rawId) {
                assert(computeCostDelta(f, g, i + f.offset, f.visited) == 0)
            }
            for (j <- g.visited.indices if g.visited(j) != move.id.rawId) {
                assert(computeCostDelta(g, f, j + g.offset, g.visited) == 0)
            }
        }
        assert(futureCosts >= 0)
        effect.a = BooleanValue(futureCosts)
        effect
    }

    override def commit(before: SearchState, after: SearchState, move: Move) = {
        for (x <- move) {
            val maybeI = f.x2i.get(x)
            if (maybeI.isDefined) {
                val i = maybeI.get
                val x = f.xs(i - f.offset)
                val jBefore = before.value(x).toInt
                if (g.indexRange.contains(jBefore)) {
                    g.refs(jBefore - g.offset) -= i
                }
                val jAfter = after.value(x).toInt
                if (g.indexRange.contains(jAfter)) {
                    g.refs(jAfter - g.offset) += i
                }
            }
            val maybeJ = g.x2i.get(x)
            if (maybeJ.isDefined) {
                val j = maybeJ.get
                val y = g.xs(j - g.offset)
                val iBefore = before.value(y).toInt
                if (f.indexRange.contains(iBefore)) {
                    f.refs(iBefore - f.offset) -= j
                }
                val iAfter = after.value(y).toInt
                if (f.indexRange.contains(iAfter)) {
                    f.refs(iAfter - f.offset) += j
                }
            }
        }
        currentCosts = futureCosts
        effect
    }

    override def isCandidateForImplicitSolving(space: Space) =
        f.isSuitableForImplicitSolving(space) &&
        g.isSuitableForImplicitSolving(space) &&
        f.xs.size == g.xs.size &&
        (f.xs.exists(! _.domain.isSingleton) || g.xs.exists(! _.domain.isSingleton)) &&
        f.xs.forall(x => x.domain.isSubsetOf(g.indexDomain)) &&
        g.xs.forall(x => x.domain.isSubsetOf(f.indexDomain))

    override def createNeighbourhood(
        space: Space,
        randomGenerator: RandomGenerator,
        moveSizeDistribution: Distribution,
        createHotSpotDistribution: Seq[AnyVariable] => Option[Distribution],
        maybeFairVariableChoiceRate: Option[Probability]):
        Option[Neighbourhood] =
    {
        if (isCandidateForImplicitSolving(space)) {
            if (f.xs.toSet.intersect(g.xs.toSet).isEmpty) {
                if (f.xs.forall(x => x.domain == g.indexDomain) &&
                    g.xs.forall(x => x.domain == f.indexDomain))
                {
                    // simplest case
                    for ((x, j) <- f.xs.iterator.zip(g.indexRange.iterator)) {
                        space.setValue(x, IntegerValue(j))
                    }
                    for ((y, i) <- g.xs.iterator.zip(f.indexRange.iterator)) {
                        space.setValue(y, IntegerValue(i))
                    }
                    space.setValue(costs, True)
                    Some(new SimpleInverseNeighbourhood(space, f, g, randomGenerator))
                }
                else if (f.xs.forall(x => x.domain.isSubsetOf(g.indexDomain)) &&
                         g.xs.forall(x => x.domain.isSubsetOf(f.indexDomain)))
                {
                    // general case
                    case class Edge(x: IntegerVariable, j: IntegerValue, y: IntegerVariable, i: IntegerValue)
                    val graph = new DefaultUndirectedGraph[IntegerVariable, Edge](classOf[Edge])
                    logger.withTimedLogScope("Building graph") {
                        for (x <- f.xs) {
                            graph.addVertex(x)
                        }
                        for (x <- g.xs) {
                            graph.addVertex(x)
                        }
                        for (i <- f.indexDomain.values) {
                            val x = f.xs(i.toInt - f.offset)
                            for (j <- x.domain.values) {
                                val y = g.xs(j.toInt - g.offset)
                                if (y.domain.contains(i)) {
                                    graph.addEdge(x, y, Edge(x, j, y, i))
                                }
                            }
                        }
                        logger.log("Added %d nodes and %d edges".format(graph.vertexSet.size, graph.edgeSet.size))
                    }
                    val (matching, _) = logger.withTimedLogScope("Computing matching") {
                        val matchingAlgo = new HopcroftKarpMaximumCardinalityBipartiteMatching(
                            graph, f.xs.toSet.asJava, g.xs.toSet.asJava)
                        matchingAlgo.getMatching
                    }
                    if (matching.getEdges.size < f.xs.size) {
                        logger.log("Unsatisfiable")
                        None
                    } else {
                        for (Edge(x, a, y, b) <- matching.getEdges.asScala) {
                            space.setValue(x, a)
                            space.setValue(y, b)
                        }
                        space.setValue(costs, True)
                        Some(new GeneralInverseNeighbourhood(space, f, g, randomGenerator))
                    }
                } else {
                    None
                }
            } else if (f.xs.size % 2 == 0 && f.xs.size > 2 && f.xs == g.xs &&
                       f.offset == g.offset && f.xs.forall(x => x.domain == f.indexDomain))
            {
                // self-inverse case, occurs in elitserien (look for RRT 5)
                for (IndexedSeq(i1, i2) <- f.indexRange.grouped(2)) {
                    space.setValue(f.xs(i1 - f.offset), IntegerValue(i2))
                    space.setValue(f.xs(i2 - f.offset), IntegerValue(i1))
                }
                space.setValue(costs, True)
                Some(new SelfInverseNeighbourhood(space, f, randomGenerator))
            } else {
                None
            }
        } else {
            None
        }
    }

    // Sometimes inverse constraints are decomposable (see elitserien, for example).
    def decompose(space: Space): Seq[Inverse] = {
        lazy val fPartitionByDomain = f.xs.groupBy(_.domain)
        lazy val gPartitionByDomain = g.xs.groupBy(_.domain)
        def domainLt(lhs: IntegerDomain, rhs: IntegerDomain) =
            lhs.lb < rhs.lb || (lhs.lb == rhs.lb && lhs.ub < rhs.ub)
        def union(lhs: IntegerDomain, rhs: IntegerDomain) = lhs.union(rhs)
        val isDecomposable =
            // the offsets are equal
            f.offset == g.offset &&
            // the variables in f have the same domains as the variables in g
            fPartitionByDomain.keysIterator.toBuffer.sortWith(domainLt) ==
                gPartitionByDomain.keysIterator.toBuffer.sortWith(domainLt) &&
            // the variables do not all have the same domain
            fPartitionByDomain.size > 1 &&
            // the domains do not overlap
            fPartitionByDomain.keysIterator.foldLeft[IntegerDomain](EmptyIntegerRange){union}.size ==
                fPartitionByDomain.keysIterator.map(_.size).sum
        if (isDecomposable) {
            fPartitionByDomain.keysIterator.map(domain => {
                val offset = domain.lb.toInt
                val costs = new BooleanVariable(space.nextVariableId(), "", CompleteBooleanDomain)
                new Inverse(
                    space.nextConstraintId(), maybeGoal,
                    new InverseFunction(fPartitionByDomain(domain).toVector, offset),
                    new InverseFunction(gPartitionByDomain(domain).toVector, offset),
                    costs,
                    logger)
            }).toList
        } else {
            List(this)
        }
    }

}

/**
 * Companion object to Inverse.
 *
 * @author Michael Marte
 */
object Inverse {

    def areInverseFunctionsOfEachOther(f: InverseFunction, g: InverseFunction, searchState: SearchState): Boolean =
        f.xs.size == g.xs.size &&
        f.indexRange.forall(i => {
            val j = searchState.value(f.xs(i - f.offset)).toInt
            g.indexRange.contains(j) && searchState.value(g.xs(j - g.offset)).value == i
        })

}
