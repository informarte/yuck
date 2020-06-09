package yuck.constraints

import scala.collection._

import yuck.core._

/**
 * Given variables x[1], ..., x[n] and an m-by-n value matrix, the constraint
 * computes the distance of (s(x[1]), ..., s(x[n])) to each row of the matrix
 * and provides the minimum distance as measure of constraint violation.
 *
 * The distance of (a[1], ..., a[n]) to (b[1], ..., b[n]) is computed as
 *   |[a[1] - b[1]| + ... + |[a[n] - b[n]|.
 *
 * @see [[yuck.Notation Notation]]
 *
 * @author Michael Marte
 */
final class IntegerTable
    (id: Id[Constraint], override val maybeGoal: Option[Goal],
     xs: immutable.IndexedSeq[IntegerVariable],
     rows: immutable.IndexedSeq[immutable.IndexedSeq[IntegerValue]],
     costs: BooleanVariable)
    extends Table(id, xs, rows, costs)
{
    import java.lang.Math.{abs, addExact => safeAdd, subtractExact => safeSub}
    override def createDomain(values: Set[IntegerValue]) =
        IntegerDomain.createDomain(values)
    override def computeDistance(a: IntegerValue, b: IntegerValue) =
        abs(safeSub(a.value.toLong, b.value.toLong))
    override def computeFutureDistances(col: IndexedSeq[IntegerValue], a0: IntegerValue, b0: IntegerValue) = {
        val a = a0.value.toLong
        val b = b0.value.toLong
        var j = 0
        val m = col.size
        while (j < m) {
            val c = col(j).value.toLong
            val bcDistance = safeSub(b, c)
            val acDistance = safeSub(a, c)
            val absBcDistance = if (bcDistance < 0) -bcDistance else bcDistance
            val absAcDistance = if (acDistance < 0) -acDistance else acDistance
            futureDistances(j) = safeAdd(futureDistances(j), safeSub(absBcDistance, absAcDistance))
            j += 1
        }
    }
}
