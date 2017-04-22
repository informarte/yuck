package yuck.constraints

import scala.collection._

import yuck.core._

/**
 * Given variables x[1], ..., x[n] and an m-by-n value matrix, the constraint
 * computes the distance of (s(x[1]), ..., s(x[n])) to each row of the matrix
 * and provides the minimum distance as measure of constraint violation.
 *
 * The distance of (a[1], ..., a[n]) to (b[1], ..., b[n]) is computed as
 * |[a[1] - b[1]| + ... + |[a[n] - b[n]|.
 *
 * A more generic implementation (based on NumericalValue) was up to three times slower.
 *
 * @see [[yuck.Notation Notation]]
 *
 * @author Michael Marte
 */
final class IntegerTable
    (id: Id[Constraint], goal: Goal,
     xs: immutable.IndexedSeq[Variable[IntegerValue]],
     rows: immutable.IndexedSeq[immutable.IndexedSeq[Int]],
     costs: Variable[IntegerValue])
    extends Constraint(id, goal)
{

    private val n = xs.size // number of columns
    rows.foreach(row => require(row.size == n))

    override def toString =
        "table([%s], [%s], %s)".format(
            xs.mkString(", "),
            rows.map(row => "[%s]".format(row.mkString(", "))).mkString(", "),
            costs)
    override def inVariables = xs
    override def outVariables = List(costs)

    // We prune infeasible rows and, to improve data locality, transpose the result.
    private lazy val cols: immutable.IndexedSeq[immutable.IndexedSeq[Int]] = {
        def f(x: Variable[IntegerValue], a: Int) =
            x.domain.contains(IntegerValue.get(a))
        val feasibleRows =
             (0 until n)
            .foldLeft(rows.toIterator){case (rows, i) => rows.filter(row => f(xs(i), row(i)))}
            .toIndexedSeq
        for (i <- 0 until n) yield for (j <- 0 until feasibleRows.size) yield feasibleRows(j)(i)
    }
    private lazy val m = cols(0).size // number of rows
    private val x2i = new immutable.HashMap[AnyVariable, Int] ++ xs.zip(0 until n)
    private var currentDistances: Array[Int] = null // for each row
    private var futureDistances: Array[Int] = null // for each row
    private val effects = List(new ReusableEffectWithFixedVariable[IntegerValue](costs))
    private val effect = effects.head

    override def initialize(now: SearchState) = {
        currentDistances = new Array[Int](m)
        futureDistances = new Array[Int](m)
        for (j <- 0 until m) {
            for (i <- 0 until n) {
                currentDistances(j) += (now.value(xs(i)).value - cols(i)(j)).abs
            }
        }
        effect.a = IntegerValue.get(currentDistances.min)
        effects
    }

    override def consult(before: SearchState, after: SearchState, move: Move) = {
        for (j <- 0 until m) {
            futureDistances(j) = currentDistances(j)
        }
        for (i <- move.involvedVariables.map(x2i)) {
            val x = xs(i)
            val a = before.value(x).value
            val b = after.value(x).value
            val col = cols(i)
            for (j <- 0 until m) {
                val c = col(j)
                futureDistances(j) += scala.math.abs(b - c) - scala.math.abs(a - c)
            }
        }
        effect.a = IntegerValue.get(futureDistances.min)
        effects
    }

    override def commit(before: SearchState, after: SearchState, move: Move) = {
        val tmp = currentDistances
        currentDistances = futureDistances
        futureDistances = tmp
        effects
    }

}
