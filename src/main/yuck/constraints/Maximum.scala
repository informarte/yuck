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
     xs: immutable.Seq[OrderedVariable[Value]], y: OrderedVariable[Value])
    (implicit valueTraits: OrderedValueTraits[Value])
    extends ValueFrequencyTracker[Value, Value](
        id, goal, xs, y,
        immutable.TreeMap[AnyVariable, Int](), immutable.TreeMap[Value, Int]())(
        valueTraits)
{
    require(! xs.isEmpty)
    override def toString = "%s = max([%s])".format(y, xs.mkString(", "))
    override protected def computeResult(searchState: SearchState, valueRegistry: ValueRegistry) =
        valueRegistry.last._1
    override def propagate = {
        val lhs0 = new Iterable[OrderedDomain[Value]] {
            override def iterator = xs.toIterator.map(x => x.domain)
        }
        val rhs0 = y.domain
        val (lhs1, rhs1) = valueTraits.domainPruner.max(lhs0, rhs0)
        NoPropagationOccurred.pruneDomains(xs.toIterator.zip(lhs1.toIterator)).pruneDomain(y, rhs1)
   }
}
