package yuck.constraints

import scala.collection._

import yuck.core._

/**
 * Given a distribution d[1], ..., d[n], scalars a[1], ..., a[n], and variables x[1], ..., x[n],
 * this constraint maintains d[i] = a[i] * x[i] for all 1 <= i <= n.
 *
 * @author Michael Marte
 */
final class DistributionMaintainer
    (id: Id[Constraint], goal: Goal,
     axs: immutable.Seq[AX[IntegerValue]], distribution: Distribution)
    extends Constraint(id, goal)
{

    assert(distribution.size == axs.size)

    override def toString = "distributionMaintainer([%s], %s)".format(axs.mkString(", "), distribution)
    override def inVariables = axs.toIterator.map(_.x)
    override def outVariables = Nil

    private val indexMap: immutable.Map[AnyVariable, (Int, AX[IntegerValue])] =
        (0 until axs.size).map(i => (axs(i).x, (i, axs(i)))).toMap

    override def initialize(now: SearchState) = {
        distribution.clear
        for (i <- 0 until axs.size) {
            distribution.setFrequency(i, axs(i).a.value * now.value(axs(i).x).value)
        }
        Nil
    }

    override def consult(before: SearchState, after: SearchState, move: Move) =
        Nil

    override def commit(before: SearchState, after: SearchState, move: Move) = {
        for (x <- move.involvedVariables) {
            val (i, ax) = indexMap(x)
            distribution.setFrequency(i, ax.a.value * after.value(ax.x).value)
        }
        Nil
    }

}
