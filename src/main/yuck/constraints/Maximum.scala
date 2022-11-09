package yuck.constraints

import scala.collection.*

import yuck.core.*

/**
 * @author Michael Marte
 *
 */
final class Maximum
    [V <: OrderedValue[V]]
    (id: Id[Constraint], override val maybeGoal: Option[Goal],
     xs: immutable.Seq[OrderedVariable[V]], y: OrderedVariable[V])
    (using valueTraits: OrderedValueTraits[V])
    extends ValueFrequencyTracker[V, V](id, xs, y)
{
    require(! xs.isEmpty)
    override def toString = "%s = max([%s])".format(y, xs.mkString(", "))
    override protected def createValueRegistry() = immutable.TreeMap[V, Int]()
    override protected def computeResult(searchState: SearchState, valueRegistry: ValueRegistry) =
        valueRegistry.last._1
    override def propagate() = {
        val lhs0 = xs.view.map(_.domain)
        val rhs0 = y.domain
        val (lhs1, rhs1) = valueTraits.domainPruner.maxRule(lhs0, rhs0)
        NoPropagationOccurred.pruneDomains(xs.iterator.zip(lhs1)).pruneDomain(y, rhs1)
   }
}
