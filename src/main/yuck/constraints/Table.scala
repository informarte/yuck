package yuck.constraints

import scala.collection.*

import yuck.core.*
import yuck.util.arm.Sigint
import yuck.util.logging.LazyLogger

/**
 * Given variables x[1], ..., x[n] and an m-by-n value matrix, the constraint
 * computes the distance of (s(x[1]), ..., s(x[n])) to each row of the matrix
 * and provides the minimum distance as measure of constraint violation.
 *
 * @see [[yuck.Notation Notation]]
 *
 * @author Michael Marte
 */
final class Table
    [V <: OrderedValue[V]]
    (id: Id[Constraint], override val maybeGoal: Option[Goal],
     xs: immutable.IndexedSeq[Variable[V]],
     private var rows: immutable.IndexedSeq[immutable.IndexedSeq[V]],
     costs: BooleanVariable,
     forceImplicitSolving: Boolean = false)
    (implicit valueTraits: OrderedValueTraits[V])
    extends Constraint(id)
{

    private val n = xs.size // number of variables/ columns
    rows.foreach(row => require(row.size == n))

    private val hasDuplicateVariables = xs.toSet.size < n

    override def toString =
        "table([%s], [%s], %s)".format(
            xs.mkString(", "),
            rows.iterator.map(row => "[%s]".format(row.mkString(", "))).mkString(", "),
            costs)

    override def inVariables = xs
    override def outVariables = List(costs)

    private var cols: immutable.IndexedSeq[immutable.IndexedSeq[V]] = null // columns improve data locality

    private var currentDistances: Array[Long] = null // for each row
    protected var futureDistances: Array[Long] = null // for each row

    private val x2i: immutable.Map[AnyVariable, Int] =
        if (hasDuplicateVariables) null else xs.iterator.zipWithIndex.toMap
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

    private val effect = costs.reuseableEffect

    private val orderingCostModel = valueTraits.orderingCostModel

    @inline private def computeDistance(a: V, b: V): Long =
        orderingCostModel.eqViolation(a, b)

    override def propagate() = {
        if (costs.domain == TrueDomain) {
            rows = rows.filter(row => (0 until n).forall(i => xs(i).domain.contains(row(i))))
            val effects =
                NoPropagationOccurred.pruneDomains(
                    (0 until n).iterator.map{i =>
                        val feasibleValues = rows.iterator.map(row => row(i)).toSet
                        val x = xs(i)
                        (x, x.domain.intersect(valueTraits.createDomain(feasibleValues)))})
            if (! effects.affectedVariables.isEmpty) {
                cols = null
            }
            effects
        } else {
            NoPropagationOccurred
        }
    }

    override def initialize(now: SearchState) = {
        val m = rows.size
        cols = rows.transpose
        currentDistances = new Array[Long](m)
        futureDistances = new Array[Long](m)
        for (j <- 0 until m) {
            val row = rows(j)
            for (i <- 0 until n) {
                currentDistances(j) =
                    safeAdd(currentDistances(j), computeDistance(now.value(xs(i)), row(i)))
            }
        }
        effect.a = BooleanValue(computeMinDistance(currentDistances))
        effect
    }

    override def consult(before: SearchState, after: SearchState, move: Move) = {
        if (cols.eq(null)) {
            initialize(before)
        }
        Array.copy(currentDistances, 0, futureDistances, 0, rows.size)
        val is =
            if (hasDuplicateVariables) {
                if (move.size == 1) x2is(move.effects.head.x).iterator
                else move.involvedVariablesIterator.flatMap(x2is)
            } else {
                if (move.size == 1) Iterator.single(x2i(move.effects.head.x))
                else move.involvedVariablesIterator.map(x2i)
            }
        while (is.hasNext) {
            val i = is.next()
            val x = xs(i)
            val a = before.value(x)
            val b = after.value(x)
            computeFutureDistances(cols(i), a, b)
        }
        effect.a = BooleanValue(computeMinDistance(futureDistances))
        effect
    }

    override def commit(before: SearchState, after: SearchState, move: Move) = {
        val tmp = currentDistances
        currentDistances = futureDistances
        futureDistances = tmp
        effect
    }

    private def computeFutureDistances(col: IndexedSeq[V], a: V, b: V): Unit = {
        var j = 0
        val m = col.size
        while (j < m) {
            val c = col(j)
            futureDistances(j) =
                safeAdd(futureDistances(j), safeSub(computeDistance(b, c), computeDistance(a, c)))
            j += 1
        }
    }

    private def computeMinDistance(distances: Array[Long]): Long = {
        var result = distances(0)
        var j = 1
        val m = rows.size
        while (j < m) {
            result = min(result, distances(j))
            j += 1
        }
        result
    }

    final override def isCandidateForImplicitSolving(space: Space) =
        rows.size > 1 &&
        xs.toSet.size == xs.size &&
        xs.forall(! space.isChannelVariable(_)) &&
        xs.forall(_.domain.isFinite) &&
        (forceImplicitSolving || xs.size <= 3)

    final override def createNeighbourhood(
        space: Space,
        randomGenerator: RandomGenerator,
        moveSizeDistribution: Distribution,
        logger: LazyLogger,
        sigint: Sigint,
        extraCfg: ExtraNeighbourhoodFactoryConfiguration):
        Option[Neighbourhood] =
    {
        if (isCandidateForImplicitSolving(space)) {
            val rows = this.rows.filter(row => (0 until n).forall(i => xs(i).domain.contains(row(i))))
            if (rows.size > 1) {
                val row = rows(randomGenerator.nextInt(rows.size))
                for ((x, a) <- xs.iterator.zip(row.iterator)) {
                    space.setValue(x, a)
                }
                space.setValue(costs, True)
                Some(new TableNeighbourhood(space, xs, rows, randomGenerator))
            } else {
                None
            }
        } else {
            None
        }
    }

}
