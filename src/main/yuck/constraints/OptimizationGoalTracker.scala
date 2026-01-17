package yuck.constraints

import scala.collection.*

import yuck.core.*


/**
 * Given a linear combination in terms of scalars a[1], ..., a[n] and variables x[1], ..., x[n],
 * this constraint maintains, for all 1 <= i <= n, the distance d[i] between the current value of
 * a[i] * x[i] and its lower or upper bound, respectively, depending on the optimization mode.
 */
final class OptimizationGoalTracker
    [A <: NumericalValue[A], D <: NumericalDomain[A, D], X <: NumericalVariable[A, D, X]]
    (id: Id[Constraint], mode: OptimizationMode, axs: immutable.IndexedSeq[AX[A, D, X]], distribution: Distribution)
    (using typeTraits: NumericalTypeTraits[A, D, X])
    extends Constraint(id)
{

    require(distribution.size == axs.size)
    require(axs.forall(_.a.toLong > Long.MinValue)) // see computeFrequency

    override def toString = "optimization_goal_tracker([%s], %s)".format(axs.mkString(", "), distribution)

    override def inVariables = axs.view.map(_.x)
    override def outVariables = Nil

    private val indexMap: HashMap[AnyVariable, (Int, AX[A, D, X])] =
        axs.indices.view.map(i => (axs(i).x, (i, axs(i)))).to(HashMap)

    override def initialize(now: SearchState) = {
        distribution.clear()
        for i <- axs.indices do {
            distribution.setFrequency(i, computeFrequency(axs(i), now))
        }
        Nil
    }

    override def consult(before: SearchState, after: SearchState, move: Move) =
        Nil

    override def commit(before: SearchState, after: SearchState, move: Move) = {
        for x <- move do {
            val (i, ax) = indexMap(x)
            distribution.setFrequency(i, computeFrequency(ax, after))
        }
        Nil
    }

    private def computeFrequency(ax: AX[A, D, X], searchState: SearchState): Long = {
        val a = ax.a.toLong
        val b = searchState.value(ax.x).toLong
        val dx = ax.x.domain
        val delta = mode match {
            case OptimizationMode.Min =>
                if ax.a < typeTraits.zero
                then safeMul(-a, safeSub(dx.ub.toLong, b)) // minimize -a * (dx.ub - x)
                else safeMul(a, safeSub(b, dx.lb.toLong)) // minimize a * (x - dx.lb)
            case OptimizationMode.Max =>
                if ax.a < typeTraits.zero
                then safeMul(-a, safeSub(b, dx.lb.toLong)) // minimize -a * (x - dx.lb)
                else safeMul(a, safeSub(dx.ub.toLong, b)) // minimize a * (dx.ub - x)
        }
        // delta may become negative when ax.x takes a value outside of its domain!
        abs(delta)
    }

}
