package yuck.constraints

import scala.collection._

import yuck.core._

/**
 * @author Michael Marte
 *
 */
final class CountVar
    [Value <: AnyValue]
    (id: Id[Constraint], goal: Goal,
     xs: immutable.Seq[Variable[Value]], y: Variable[Value], n: Variable[IntegerValue])
    (implicit valueTraits: AnyValueTraits[Value])
    extends ValueFrequencyTracker[Value, IntegerValue](
        id, goal, xs, n,
        immutable.TreeMap[AnyVariable, Int](), immutable.HashMap[Value, Int]())(
        valueTraits)
{
    override def toString = "%s = count(%s, [%s])".format(n, y, xs.mkString(", "))
    override def inVariables = xs.toIterator ++ List(y).toIterator
    override def todo(move: Move) = super.todo(move).filter(x => x != y)
    override protected def computeResult(searchState: SearchState, valueRegistry: ValueRegistry) =
        IntegerValue.get(valueRegistry.getOrElse(searchState.value(y), 0))
}
