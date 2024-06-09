package yuck.constraints

import scala.collection.*

import yuck.core.*

/**
 * @author Michael Marte
 *
 */
final class Minimum
    [V <: OrderedValue[V]]
    (id: Id[Constraint],
     override val maybeGoal: Option[Goal],
     override protected val xs: immutable.Seq[OrderedVariable[V]],
     override protected val result: OrderedVariable[V])
    (using override protected val valueTraits: OrderedValueTraits[V])
    extends ValueFrequencyTracker[V, V](id)
{
    require(! xs.isEmpty)
    override def toString = "%s = min([%s])".format(result, xs.mkString(", "))
    override protected def createValueRegistry() = immutable.TreeMap[V, Int]()
    override protected def computeResult(searchState: SearchState, valueRegistry: ValueRegistry) =
        valueRegistry.head._1
    override def propagate() = {
        val lhs0 = xs.view.map(_.domain)
        val rhs0 = result.domain
        val (lhs1, rhs1) = valueTraits.domainPruner.minRule(lhs0, rhs0)
        NoPropagationOccurred.pruneDomains(xs.iterator.zip(lhs1)).pruneDomain(result, rhs1)
    }
}
