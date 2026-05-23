package yuck.constraints

import scala.collection.*

import yuck.core.*

/**
 * Computes the violation of sum x(i) R z where R is an ordering relation.
 *
 * y is a helper channel for propagation: Conceptually, sum a(i) * x(i) = y /\ y R z.
 */
final class SumConstraint
    [A <: NumericalValue[A], D <: NumericalDomain[A, D], X <: NumericalVariable[A, D, X]]
    (id: Id[Constraint],
     val xs: immutable.IndexedSeq[X],
     override val y: X,
     override val relation: OrderingRelation,
     override val z: X,
     override val costs: BooleanVariable)
    (using override protected val typeTraits: NumericalTypeTraits[A, D, X])
    extends LinearConstraintLike[A, D, X](id)
{

    require(xs.toSet.size == xs.size)

    override protected val n = xs.size
    override protected def a(i: Int) = typeTraits.one
    override protected def x(i: Int) = xs(i)

    override def copy(replacements: Map[AnyVariable, AnyVariable]) =
        new SumConstraint(id, xs, y, relation, z, replacements.getOrElse(costs, costs).asInstanceOf[BooleanVariable])

    override def consult(before: SearchState, after: SearchState, move: Move) = {
        futureSum = currentSum
        for x0 <- move do {
            if x0 != z then {
                val x = typeTraits.safeDowncast(x0)
                futureSum = futureSum.addAndSub(after.value(x), before.value(x))
            }
        }
        effect.a = computeCosts(futureSum, after.value(z))
        effect
    }

}
