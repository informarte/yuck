package yuck.constraints

import scala.collection.*

import yuck.core.*

/**
 * Computes the violation of sum a(i) * x(i) R z where R is an ordering relation.
 *
 * y is a helper variable for propagation: Conceptually, sum a(i) * x(i) = y /\ y R z.
 */
final class LinearConstraint
    [A <: NumericalValue[A], D <: NumericalDomain[A, D], X <: NumericalVariable[A, D, X]] private
    (id: Id[Constraint],
     val axs: immutable.IndexedSeq[AX[A, D, X]],
     override val y: X,
     override val relation: OrderingRelation,
     override val z: X,
     override val costs: BooleanVariable,
     x2ax: HashMap[AnyVariable, AX[A, D, X]])
    (using override protected val typeTraits: NumericalTypeTraits[A, D, X])
    extends LinearConstraintLike[A, D, X](id)
{

    require(axs.iterator.map(_.x).toSet.size == axs.size)

    override protected val n = axs.size
    override protected def a(i: Int) = axs(i).a
    override protected def x(i: Int) = axs(i).x

    def this
        (id: Id[Constraint], axs: immutable.IndexedSeq[AX[A, D, X]], y: X, relation: OrderingRelation, z: X, costs: BooleanVariable)
        (using typeTraits: NumericalTypeTraits[A, D, X]) =
    {
        this(id, axs, y, relation, z, costs, axs.view.map(ax => ax.x -> ax).to(HashMap))
    }

    override def copy(replacements: Map[AnyVariable, AnyVariable]) =
        new LinearConstraint(id, axs, y, relation, z, replacements.getOrElse(costs, costs).asInstanceOf[BooleanVariable], x2ax)

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
