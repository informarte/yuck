package yuck.constraints

import scala.collection._

import yuck.core._

/**
 * Implements the ''alldifferent_except_0'' constraint as specified by MiniZinc.
 *
 * Given a set X of variables, the constraint maintains the set A = {s(x): x in X} of values
 * assigned to the variables and provides |{x in X: s(x) <> 0}| - |A \ {0}| as measure of
 * constraint violation.
 *
 * @see [[yuck.Notation Notation]]
 *
 * @author Michael Marte
 */
final class AlldistinctExceptZero
    [Value <: NumericalValue[Value]]
    (id: Id[Constraint], goal: Goal,
     xs: immutable.Seq[Variable[Value]], costs: Variable[IntegerValue])
    (implicit valueTraits: NumericalValueTraits[Value])
    extends ValueFrequencyTracker[Value, IntegerValue](
        id, goal, xs, costs,
        immutable.TreeMap[AnyVariable, Int](), immutable.HashMap[Value, Int]())(
        valueTraits)
{
    override def toString = "alldistinctExceptZero([%s], %s)".format(xs.mkString(", "), costs)
    override protected def computeResult(searchState: SearchState, valueRegistry: ValueRegistry) = {
        val maybeZeroCount = valueRegistry.get(valueTraits.zero)
        IntegerValue.get((xs.size - maybeZeroCount.getOrElse(0)) -
        (valueRegistry.size - (if (maybeZeroCount.isEmpty) 0 else 1)))
    }
}
