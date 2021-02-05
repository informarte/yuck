package yuck.constraints

import scala.collection._

import yuck.core._

/**
 * Given variables x[1], ..., x[n] and an m-by-n value matrix, the constraint
 * computes the distance of (s(x[1]), ..., s(x[n])) to each row of the matrix
 * and provides the minimum distance as measure of constraint violation.
 *
 * @see [[yuck.Notation Notation]]
 *
 * @author Michael Marte
 */
abstract class Table
    [Value <: AnyValue]
    (id: Id[Constraint],
     xs: immutable.IndexedSeq[Variable[Value]],
     private var rows: immutable.IndexedSeq[immutable.IndexedSeq[Value]],
     costs: BooleanVariable)
    extends Constraint(id)
{

    private val n = xs.size // number of variables/ columns
    rows.foreach(row => require(row.size == n))

    private val hasDuplicateVariables = xs.toSet.size < n

    final override def toString =
        "table([%s], [%s], %s)".format(
            xs.mkString(", "),
            rows.iterator.map(row => "[%s]".format(row.mkString(", "))).mkString(", "),
            costs)

    final override def inVariables = xs
    final override def outVariables = List(costs)

    private var cols: immutable.IndexedSeq[immutable.IndexedSeq[Value]] = null // columns improve data locality

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

    private val effect = new ReusableMoveEffectWithFixedVariable(costs)

    protected def createDomain(values: Set[Value]): Domain[Value]
    protected def computeDistance(a: Value, b: Value): Long

    final override def propagate = {
        if (costs.domain == TrueDomain) {
            rows = rows.filter(row => (0 until n).forall(i => xs(i).domain.contains(row(i))))
            val effects =
                NoPropagationOccurred.pruneDomains(
                    (0 until n).iterator.map{i =>
                        val feasibleValues = rows.iterator.map(row => row(i)).toSet
                        val x = xs(i)
                        (x, x.domain.intersect(createDomain(feasibleValues)))})
            if (! effects.affectedVariables.isEmpty) {
                cols = null
            }
            effects
        } else {
            NoPropagationOccurred
        }
    }

    final override def initialize(now: SearchState) = {
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

    protected def computeFutureDistances(col: IndexedSeq[Value], a: Value, b: Value): Unit = {
        var j = 0
        val m = col.size
        while (j < m) {
            val c = col(j)
            futureDistances(j) =
                safeAdd(futureDistances(j), safeSub(computeDistance(b, c), computeDistance(a, c)))
            j += 1
        }
    }

    final override def consult(before: SearchState, after: SearchState, move: Move) = {
        if (cols == null) {
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
            val i = is.next
            val x = xs(i)
            val a = before.value(x)
            val b = after.value(x)
            computeFutureDistances(cols(i), a, b)
        }
        effect.a = BooleanValue(computeMinDistance(futureDistances))
        effect
    }

    final override def commit(before: SearchState, after: SearchState, move: Move) = {
        val tmp = currentDistances
        currentDistances = futureDistances
        futureDistances = tmp
        effect
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

}
