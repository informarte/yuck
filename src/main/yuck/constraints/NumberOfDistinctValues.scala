package yuck.constraints

import scala.collection._

import yuck.core._

/**
 * @author Michael Marte
 *
 */
class NumberOfDistinctValues
    [Value <: OrderedValue[Value]]
    (id: Id[Constraint], override val maybeGoal: Option[Goal],
     xs: immutable.Seq[Variable[Value]], n: IntegerVariable)
    (implicit valueTraits: OrderedValueTraits[Value])
    extends ValueFrequencyTracker[Value, IntegerValue](
        id, xs, n,
        immutable.TreeMap[AnyVariable, Int](), immutable.HashMap[Value, Int]())(
        valueTraits)
{
    override def toString = "%s = numberOfDistinctValues([%s])".format(n, xs.mkString(", "))
    override protected def computeResult(searchState: SearchState, valueRegistry: ValueRegistry) =
        IntegerValue(valueRegistry.size)
}
