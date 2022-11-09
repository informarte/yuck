package yuck.constraints

import scala.collection.*

import yuck.core.*

/**
 * @author Michael Marte
 *
 */
final class CountVar
    [V <: AnyValue]
    (id: Id[Constraint], override val maybeGoal: Option[Goal],
     xs: immutable.Seq[Variable[V]], y: Variable[V], n: IntegerVariable)
    (using valueTraits: ValueTraits[V])
    extends ValueFrequencyTracker[V, IntegerValue](id, xs, n)
{

    override def toString = "%s = count(%s, [%s])".format(n, y, xs.mkString(", "))

    override def inVariables = xs.view :+ y
    override def todo(move: Move) = super.todo(move).filter(x => x != y)
    override protected def computeResult(searchState: SearchState, valueRegistry: ValueRegistry) =
        IntegerValue(valueRegistry.getOrElse(valueTraits.normalizedValue(searchState.value(y)), 0))

}
