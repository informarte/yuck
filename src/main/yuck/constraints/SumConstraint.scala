package yuck.constraints

import scala.collection.*

import yuck.core.*

/**
 * Computes the violation of sum x(i) R z where R is an ordering relation.
 *
 * y is a helper channel for propagation: Conceptually, sum a(i) * x(i) = y /\ y R z.
 *
 * @author Michael Marte
 */
final class SumConstraint
    [V <: NumericalValue[V]]
    (id: Id[Constraint], override val maybeGoal: Option[Goal],
     xs: immutable.IndexedSeq[NumericalVariable[V]],
     override protected val y: NumericalVariable[V],
     override protected val relation: OrderingRelation,
     override protected val z: NumericalVariable[V],
     override protected val costs: BooleanVariable)
    (using override protected val valueTraits: NumericalValueTraits[V])
    extends LinearConstraintLike[V](id)
{

    require(xs.toSet.size == xs.size)

    override protected val n = xs.size
    override protected def a(i: Int) = valueTraits.one
    override protected def x(i: Int) = xs(i)

    override def consult(before: SearchState, after: SearchState, move: Move) = {
        futureSum = currentSum
        for (x0 <- move) {
            if (x0 != z) {
                val x = valueTraits.safeDowncast(x0)
                futureSum = futureSum.addAndSub(after.value(x), before.value(x))
            }
        }
        effect.a = computeCosts(futureSum, after.value(z))
        effect
    }

}
