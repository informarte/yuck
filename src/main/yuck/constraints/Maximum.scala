package yuck.constraints

import scala.collection._

import yuck.core._

/**
 * @author Michael Marte
 *
 */
final class Maximum
    [Value <: OrderedValue[Value]]
    (id: Id[Constraint], goal: Goal,
     xs: immutable.Seq[Variable[Value]], max: Variable[Value])
    (implicit valueTraits: OrderedValueTraits[Value])
    extends ValueFrequencyTracker[Value, Value](
        id, goal, xs, max,
        immutable.TreeMap[AnyVariable, Int](), immutable.TreeMap[Value, Int]())(
        valueTraits)
{
    override def toString = "%s = max([%s])".format(max, xs.mkString(", "))
    override protected def computeResult(searchState: SearchState, valueRegistry: ValueRegistry) =
        valueRegistry.last._1
}
