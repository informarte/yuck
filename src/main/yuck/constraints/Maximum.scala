package yuck.constraints

import scala.collection._

import yuck.core._

/**
 * @author Michael Marte
 *
 */
final class Maximum
    [V <: OrderedValue[V]]
    (id: Id[Constraint], override val maybeGoal: Option[Goal],
     xs: immutable.Seq[OrderedVariable[V]], y: OrderedVariable[V])
    (implicit valueTraits: OrderedValueTraits[V])
    extends ValueFrequencyTracker[V, V](
        id, xs, y,
        immutable.TreeMap[AnyVariable, Int](), immutable.TreeMap[V, Int]())(
        valueTraits)
{
    require(! xs.isEmpty)
    override def toString = "%s = max([%s])".format(y, xs.mkString(", "))
    override protected def computeResult(searchState: SearchState, valueRegistry: ValueRegistry) =
        valueRegistry.last._1
    override def propagate = {
        val lhs0 = new Iterable[OrderedDomain[V]] {
            override def iterator = xs.iterator.map(x => x.domain)
        }
        val rhs0 = y.domain
        val (lhs1, rhs1) = valueTraits.domainPruner.maxRule(lhs0, rhs0)
        NoPropagationOccurred.pruneDomains(xs.iterator.zip(lhs1.iterator)).pruneDomain(y, rhs1)
   }
}
