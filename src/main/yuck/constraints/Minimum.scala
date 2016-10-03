package yuck.constraints

import scala.collection._

import yuck.core._

/**
 * @author Michael Marte
 *
 */
final class Minimum
    [Value <: OrderedValue[Value]]
    (id: Id[Constraint], goal: Goal,
     xs: immutable.Seq[Variable[Value]], min: Variable[Value])
    (implicit valueTraits: OrderedValueTraits[Value])
    extends ValueFrequencyTracker[Value, Value](
        id, goal, xs, min,
        immutable.TreeMap[AnyVariable, Int](), immutable.TreeMap[Value, Int]())(
        valueTraits)
{
    override def toString = "%s = min([%s])".format(min, xs.mkString(", "))
    override protected def computeResult(searchState: SearchState, valueRegistry: ValueRegistry) =
        valueRegistry.head._1
}
