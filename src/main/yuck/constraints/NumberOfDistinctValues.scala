package yuck.constraints

import scala.collection.*

import yuck.core.*

/**
 * @author Michael Marte
 *
 */
class NumberOfDistinctValues
    [V <: OrderedValue[V]]
    (id: Id[Constraint], override val maybeGoal: Option[Goal],
     xs: immutable.Seq[Variable[V]], n: IntegerVariable)
    (using valueTraits: OrderedValueTraits[V])
    extends ValueFrequencyTracker[V, IntegerValue](id, xs, n)
{
    override def toString = "%s = numberOfDistinctValues([%s])".format(n, xs.mkString(", "))
    override protected def computeResult(searchState: SearchState, valueRegistry: ValueRegistry) =
        IntegerValue(valueRegistry.size)
}
