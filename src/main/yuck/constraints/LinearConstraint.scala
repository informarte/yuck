package yuck.constraints

import scala.collection.*

import yuck.core.*

/**
 * Computes the violation of sum a(i) * x(i) R z where R is an ordering relation.
 *
 * y is a helper variable for propagation: Conceptually, sum a(i) * x(i) = y /\ y R z.
 */
final class LinearConstraint
    [A <: NumericalValue[A], D <: NumericalDomain[A, D], X <: NumericalVariable[A, D, X]]
    (id: Id[Constraint], override val maybeGoal: Option[Goal],
     axs: immutable.IndexedSeq[AX[A, D, X]],
     override protected val y: X,
     override protected val relation: OrderingRelation,
     override protected val z: X,
     override protected val costs: BooleanVariable)
    (using override protected val typeTraits: NumericalTypeTraits[A, D, X])
    extends LinearConstraintLike[A, D, X](id)
{

    require(axs.iterator.map(_.x).toSet.size == axs.size)

    override protected val n = axs.size
    override protected def a(i: Int) = axs(i).a
    override protected def x(i: Int) = axs(i).x

    private val x2ax: HashMap[AnyVariable, AX[A, D, X]] = axs.view.map(ax => ax.x -> ax).to(HashMap)

    override def consult(before: SearchState, after: SearchState, move: Move) = {
        futureSum = currentSum
        for x0 <- move do {
            if x0 != z then {
                val ax = x2ax(x0)
                val x = ax.x
                futureSum = futureSum.addAndSub(ax.a, after.value(x), before.value(x))
            }
        }
        effect.a = computeCosts(futureSum, after.value(z))
        effect
    }

}
