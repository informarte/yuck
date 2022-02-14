package yuck.constraints

import scala.collection.*

import yuck.core.*

/**
 * Computes the violation of sum a(i) * x(i) R z where R is an ordering relation.
 *
 * y is a helper variable for propagation: Conceptually, sum a(i) * x(i) = y /\ y R z.
 *
 * @author Michael Marte
 */
final class LinearConstraint
    [V <: NumericalValue[V]]
    (id: Id[Constraint], override val maybeGoal: Option[Goal],
     axs: immutable.IndexedSeq[AX[V]],
     override protected val y: NumericalVariable[V],
     override protected val relation: OrderingRelation,
     override protected val z: NumericalVariable[V],
     override protected val costs: BooleanVariable)
    (implicit override protected val valueTraits: NumericalValueTraits[V])
    extends LinearConstraintLike[V](id)
{

    require(axs.iterator.map(_.x).toSet.size == axs.size)

    override protected val n = axs.size
    override protected def a(i: Int) = axs(i).a
    override protected def x(i: Int) = axs(i).x

    private val x2i: immutable.Map[AnyVariable, Int] =
        new immutable.HashMap[AnyVariable, Int] ++ (axs.iterator.map(_.x).zipWithIndex)

    override def consult(before: SearchState, after: SearchState, move: Move) = {
        futureSum = currentSum
        for (x0 <- move) {
            if (x0 != z) {
                val i = x2i(x0)
                val ax = axs(i)
                val x = ax.x
                futureSum = futureSum.addAndSub(ax.a, after.value(x), before.value(x))
            }
        }
        effect.a = computeCosts(futureSum, after.value(z))
        effect
    }

}
