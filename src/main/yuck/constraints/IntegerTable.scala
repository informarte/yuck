package yuck.constraints

import scala.collection._
import scala.math.abs

import yuck.core._

/**
 * Given variables x[1], ..., x[n] and an m-by-n value matrix, the constraint
 * computes the distance of (s(x[1]), ..., s(x[n])) to each row of the matrix
 * and provides the minimum distance as measure of constraint violation.
 *
 * The distance of (a[1], ..., a[n]) to (b[1], ..., b[n]) is computed as
 *   |[a[1] - b[1]| + ... + |[a[n] - b[n]|.
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
     costs: Variable[BooleanValue])
    extends Constraint(id, goal)
{

    private val n = xs.size // number of variables/ columns
    rows.foreach(row => require(row.size == n))

    override def toString =
        "table([%s], [%s], %s)".format(
            xs.mkString(", "),
            rows.map(row => "[%s]".format(row.mkString(", "))).mkString(", "),
            costs)
    override def inVariables = xs
    override def outVariables = List(costs)


    private var feasibleRows = rows
    private lazy val cols = feasibleRows.transpose // columns improve data locality
    private lazy val m = feasibleRows.size
    private val x2i = new immutable.HashMap[AnyVariable, Int] ++ xs.zip(0 until n)
    private var currentDistances: Array[Int] = null // for each row
    private var futureDistances: Array[Int] = null // for each row
    private val effects = List(new ReusableEffectWithFixedVariable[BooleanValue](costs))
    private val effect = effects.head

    override def propagate = {
        if (costs.domain == TrueDomain) {
            feasibleRows =
                feasibleRows.filter(row => (0 until n).forall(i => xs(i).domain.contains(IntegerValue.get(row(i)))))
            Variable.pruneDomains(
                for (i <- 0 until n) yield {
                    val feasibleValues = feasibleRows.toIterator.map(row => IntegerValue.get(row(i))).toSet
                    val x = xs(i)
                    (x, x.domain.intersect(IntegerDomain.createDomain(feasibleValues)))
                }
            )
        } else {
            false
        }
    }

    override def initialize(now: SearchState) = {
        currentDistances = new Array[Int](m)
        futureDistances = new Array[Int](m)
        for (j <- 0 until m) {
            for (i <- 0 until n) {
                currentDistances(j) =
                    safeAdd(currentDistances(j), abs(safeSub(now.value(xs(i)).value, cols(i)(j))))
            }
        }
        effect.a = BooleanValue.get(currentDistances.min)
        effects
    }

    override def consult(before: SearchState, after: SearchState, move: Move) = {
        Array.copy(currentDistances, 0, futureDistances, 0, m)
        for (x0 <- move.involvedVariables) {
            val i = x2i(x0)
            val x = xs(i)
            val a = before.value(x).value
            val b = after.value(x).value
            val col = cols(i)
            var j = 0
            while (j < m) {
                val c = col(j)
                futureDistances(j) =
                    safeAdd(futureDistances(j), safeSub(abs(safeSub(b, c)), abs(safeSub(a, c))))
                j += 1
            }
        }
        effect.a = BooleanValue.get(futureDistances.min)
        effects
    }

    override def commit(before: SearchState, after: SearchState, move: Move) = {
        val tmp = currentDistances
        currentDistances = futureDistances
        futureDistances = tmp
        effects
    }

}
