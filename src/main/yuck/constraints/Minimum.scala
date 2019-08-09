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
     xs: immutable.Seq[OrderedVariable[Value]], y: OrderedVariable[Value])
    (implicit valueTraits: OrderedValueTraits[Value])
    extends ValueFrequencyTracker[Value, Value](
        id, goal, xs, y,
        immutable.TreeMap[AnyVariable, Int](), immutable.TreeMap[Value, Int]())(
        valueTraits)
{
    require(! xs.isEmpty)
    override def toString = "%s = min([%s])".format(y, xs.mkString(", "))
    override protected def computeResult(searchState: SearchState, valueRegistry: ValueRegistry) =
        valueRegistry.head._1
    override def propagate = {
        val lhs0 = new Iterable[OrderedDomain[Value]] {
            override def iterator = xs.iterator.map(_.domain)
        }
        val rhs0 = y.domain
        val (lhs1, rhs1) = valueTraits.domainPruner.min(lhs0, rhs0)
        Variable.pruneDomains(xs.view.zip(lhs1.view)) ||| y.pruneDomain(rhs1)
    }
}
