package yuck.constraints

import scala.collection.*

import yuck.core.*

class NumberOfDistinctValues
    [A <: OrderedValue[A], D <: OrderedDomain[A, D], X <: OrderedVariable[A, D, X]]
    (id: Id[Constraint],
     override val maybeGoal: Option[Goal],
     override protected val xs: immutable.Seq[X],
     override protected val result: IntegerVariable)
    (using override protected val typeTraits: OrderedTypeTraits[A, D, X])
    extends ValueFrequencyTracker[A, D, X, IntegerValue, IntegerDomain, IntegerVariable](id)
{
    override def toString = "%s = nvalue([%s])".format(result, xs.mkString(", "))
    override protected def computeResult(searchState: SearchState, valueRegistry: ValueRegistry) =
        IntegerValue(valueRegistry.size)
    override def propagate() =
        NoPropagationOccurred.pruneDomain(result, IntegerRange(if xs.isEmpty then 0 else 1, xs.size))
}
