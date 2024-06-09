package yuck.constraints

import scala.collection.*

import yuck.core.*

/**
 * @author Michael Marte
 *
 */
class NumberOfDistinctValues
    [V <: OrderedValue[V]]
    (id: Id[Constraint],
     override val maybeGoal: Option[Goal],
     override protected val xs: immutable.Seq[Variable[V]],
     override protected val result: IntegerVariable)
    (using override protected val valueTraits: OrderedValueTraits[V])
    extends ValueFrequencyTracker[V, IntegerValue](id)
{
    override def toString = "%s = nvalue([%s])".format(result, xs.mkString(", "))
    override protected def computeResult(searchState: SearchState, valueRegistry: ValueRegistry) =
        IntegerValue(valueRegistry.size)
    override def propagate() =
        NoPropagationOccurred.pruneDomain(result, IntegerRange(if (xs.isEmpty) 0 else 1, xs.size))
}
