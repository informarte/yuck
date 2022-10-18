package yuck.constraints

import scala.collection.*

import yuck.core.*

/**
 * Implements the ''alldifferent_except'' constraint as specified by MiniZinc.
 *
 * Given a set X of variables and a set of values S, the constraint maintains the set A = {s(x): x in X}
 * of values assigned to the variables and provides |{x in X: s(x) not in S}| - |A \ S| as measure of
 * constraint violation.
 *
 * @see [[yuck.Notation Notation]]
 *
 * @author Michael Marte
 */
final class AlldistinctExcept
    [V <: NumericalValue[V]]
    (id: Id[Constraint], override val maybeGoal: Option[Goal],
     xs: immutable.Seq[NumericalVariable[V]], S: immutable.Set[V], costs: BooleanVariable)
    (implicit valueTraits: NumericalValueTraits[V])
    extends ValueFrequencyTracker[V, BooleanValue](id, xs, costs)
{
    override def toString = "alldistinctExcept([%s], {%s}, %s)".format(xs.mkString(", "), S.mkString(", "), costs)
    override protected def computeResult(searchState: SearchState, valueRegistry: ValueRegistry) = {
        var violation = xs.size - valueRegistry.size
        for (a <- S) {
            val maybeCount = valueRegistry.get(a)
            if (maybeCount.isDefined) {
                violation -= maybeCount.get - 1
            }
        }
        BooleanValue(violation)
    }
}
