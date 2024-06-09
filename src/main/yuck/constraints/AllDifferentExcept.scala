package yuck.constraints

import scala.collection.*

import yuck.core.*
import yuck.util.logging.LazyLogger

/**
 * Given a set X of variables and a set of values S, the constraint maintains the set A = {s(x): x in X}
 * of values assigned to the variables and provides |{x in X: s(x) not in S}| - |A \ S| as measure of
 * constraint violation.
 *
 * @see [[yuck.Notation Notation]]
 *
 * @author Michael Marte
 */
final class AllDifferentExcept
    [V <: Value[V]]
    (id: Id[Constraint],
     override val maybeGoal: Option[Goal],
     override protected val xs: immutable.Seq[Variable[V]],
     S: immutable.Set[V],
     override protected val result: BooleanVariable)
    (using override protected val valueTraits: ValueTraits[V])
    extends ValueFrequencyTracker[V, BooleanValue](id)
{

    final override def toString = "all_different_except([%s], {%s}, %s)".format(xs.mkString(", "), S.mkString(", "), result)

    final override protected def computeResult(searchState: SearchState, valueRegistry: ValueRegistry) = {
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
