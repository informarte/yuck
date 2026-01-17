package yuck.constraints

import scala.collection.*

import yuck.core.*

final class Maximum
    [A <: OrderedValue[A], D <: OrderedDomain[A, D], X <: OrderedVariable[A, D, X]]
    (id: Id[Constraint],
     override protected val xs: immutable.Seq[X],
     override protected val result: X)
    (using override protected val typeTraits: OrderedTypeTraits[A, D, X])
    extends ValueFrequencyTracker[A, D, X, A, D, X](id)
{
    require(! xs.isEmpty)
    override def toString = "%s = max([%s])".format(result, xs.mkString(", "))
    override protected def createValueRegistry() = TreeMap[A, Int]()
    override protected def computeResult(searchState: SearchState, valueRegistry: ValueRegistry) =
        valueRegistry.last._1
    override def propagate() = {
        val lhs0 = xs.view.map(_.domain)
        val rhs0 = result.domain
        val (lhs1, rhs1) = typeTraits.domainPruner.maxRule(lhs0, rhs0)
        NoPropagationOccurred.pruneDomains(xs.iterator.zip(lhs1)).pruneDomain(result, rhs1)
   }
}
