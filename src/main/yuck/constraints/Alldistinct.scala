package yuck.constraints

import scala.collection._

import yuck.core._

/**
 * Implements the ''all_different_int'' constraint as specified by MiniZinc.
 *
 * Given a set X of variables, the constraint maintains the set A = {s(x): x in X} of values
 * assigned to the variables and provides |X| - |A| as measure of constraint violation.
 *
 * @see [[yuck.Notation Notation]]
 *
 * @author Michael Marte
 */
final class Alldistinct
    [Value <: AnyValue]
    (id: Id[Constraint], goal: Goal,
     val xs: immutable.Seq[Variable[Value]], costs: Variable[IntegerValue])
    (implicit valueTraits: AnyValueTraits[Value])
    extends ValueFrequencyTracker[Value, IntegerValue](
        id, goal, xs, costs,
        immutable.TreeMap[AnyVariable, Int](), immutable.HashMap[Value, Int]())(
        valueTraits)
{

    override def toString = "alldistinct([%s], %s)".format(xs.mkString(", "), costs)

    override protected def computeResult(searchState: SearchState, valueRegistry: ValueRegistry) =
        IntegerValue.get(xs.size - valueRegistry.size)

    /**
     * We call an Alldistinct constraint ''tight'' when all its variables (excluding parameters)
     * are search variables, they have the same domain, and the domain size equals the number of variables.
     */
    def isTight(space: Space): Boolean = {
        val xs = this.xs.filter(_.isVariable)
        xs.size > 1 &&
        xs.forall(x => space.isSearchVariable(x)) &&
        {
            val domain = xs.head.domain
            domain.isFinite && domain.size == xs.size && xs.forall(_.domain == domain)
        }
    }

}
