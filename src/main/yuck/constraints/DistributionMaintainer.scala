package yuck.constraints

import scala.collection._
import scala.math.abs

import yuck.core._


/**
 * Given a linear combination in terms of scalars a[1], ..., a[n] and variables x[1], ..., x[n],
 * this constraint maintains, for all 1 <= i <= n, the distance d[i] between the current value of
 * a[i] * x[i] and its lower or upper bound, respectively, depending on the optimization mode.
 *
 * @author Michael Marte
 */
final class DistributionMaintainer
    (id: Id[Constraint], goal: Goal,
     mode: OptimizationMode.Value,
     axs: immutable.Seq[AX[IntegerValue]], distribution: Distribution)
    extends Constraint(id, goal)
{

    require(distribution.size == axs.size)
    require(axs.forall(_.a.value > Int.MinValue)) // see computeFrequency

    override def toString = "distributionMaintainer([%s], %s)".format(axs.mkString(", "), distribution)
    override def inVariables = axs.toIterator.map(_.x)
    override def outVariables = Nil

    private val indexMap: immutable.Map[AnyVariable, (Int, AX[IntegerValue])] =
        (0 until axs.size).map(i => (axs(i).x, (i, axs(i)))).toMap

    override def initialize(now: SearchState) = {
        distribution.clear
        for (i <- 0 until axs.size) {
            distribution.setFrequency(i, computeFrequency(axs(i), now))
        }
        Nil
    }

    override def consult(before: SearchState, after: SearchState, move: Move) =
        Nil

    override def commit(before: SearchState, after: SearchState, move: Move) = {
        for (x <- move.involvedVariables) {
            val (i, ax) = indexMap(x)
            distribution.setFrequency(i, computeFrequency(ax, after))
        }
        Nil
    }

    private def computeFrequency(ax: AX[IntegerValue], searchState: SearchState): Int = {
        val a = ax.a.value
        val b = searchState.value(ax.x).value
        val dx = IntegerValueTraits.safeDowncast(ax.x.domain)
        val delta = mode match {
            case OptimizationMode.Min =>
                if (ax.a < Zero) safeMul(-a, safeSub(dx.ub.value, b)) // minimize -a * (dx.ub - x)
                else safeMul(a, safeSub(b, dx.lb.value)) // minimize a * (x - dx.lb)
            case OptimizationMode.Max =>
                if (ax.a < Zero) safeMul(-a, safeSub(b, dx.lb.value)) // minimize -a * (x - dx.lb)
                else safeMul(a, safeSub(dx.ub.value, b)) // minimize a * (dx.ub - x)
        }
        // delta may become negative when ax.x takes a value outside of its domain!
        abs(delta)
    }

}
