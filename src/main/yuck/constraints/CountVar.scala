package yuck.constraints

import scala.collection.*

import yuck.core.*

/**
 * @author Michael Marte
 *
 */
final class CountVar
    [V <: Value[V]]
    (id: Id[Constraint],
     override val maybeGoal: Option[Goal],
     override protected val xs: immutable.Seq[Variable[V]],
     y: Variable[V],
     override protected val result: IntegerVariable)
    (using override protected val valueTraits: ValueTraits[V])
    extends ValueFrequencyTracker[V, IntegerValue](id)
{

    override def toString = "%s = count(%s, [%s])".format(result, y, xs.mkString(", "))

    override def inVariables = xs.view :+ y
    override def todo(move: Move) = super.todo(move).filter(x => x != y)
    override protected def computeResult(searchState: SearchState, valueRegistry: ValueRegistry) =
        IntegerValue(valueRegistry.getOrElse(valueTraits.normalizedValue(searchState.value(y)), 0))

}
