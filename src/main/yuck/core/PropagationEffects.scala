package yuck.core

import scala.collection._

/**
  * The effects of a propagation step.
  *
  * Remembers the variables the domains of which were pruned and knows whether the step needs rescheduling.
  *
  * Don't prune domains directly; instead use the pruning methods provided by this class!
  *
  * @author Michael Marte
  */
abstract class PropagationEffects {

    /** The variables the domains of which were pruned. */
    val affectedVariables: TraversableOnce[AnyVariable]

    /** Whether the step needs rescheduling. */
    def rescheduleStep: Boolean

    /** Prunes the domain of the given variable and records the event. */
    def pruneDomain[Value <: AnyValue](x: Variable[Value], dx: Domain[Value]): PropagationEffects

    /** Prunes the domains of the given variables and records the events. */
    def pruneDomains
        [Value1 <: AnyValue, Value2 <: AnyValue]
        (x: Variable[Value1], dx: Domain[Value1], y: Variable[Value2], dy: Domain[Value2]):
        PropagationEffects =
    {
        pruneDomain(x, dx).pruneDomain(y, dy)
    }

    /** Prunes the domains of the given variables and records the events. */
    def pruneDomains
        [Value1 <: AnyValue, Value2 <: AnyValue, Value3 <: AnyValue]
        (x: Variable[Value1], dx: Domain[Value1], y: Variable[Value2], dy: Domain[Value2], z: Variable[Value3], dz: Domain[Value3]):
        PropagationEffects =
    {
        pruneDomain(x, dx).pruneDomain(y, dy).pruneDomain(z, dz)
    }

    /** Prunes the domains of the given variables and records the events. */
    def pruneDomains
        [Value <: AnyValue]
        (xds: TraversableOnce[(Variable[Value], Domain[Value])]):
        PropagationEffects =
    {
        xds.foldLeft(this){case (result, (x, dx)) => result.pruneDomain(x, dx)}
    }

}

/**
  * Starting point for constraint propagation.
  *
  * @author Michael Marte
  */
case object NoPropagationOccurred extends PropagationEffects {
    override val affectedVariables = None
    override def rescheduleStep = false
    override def pruneDomain[Value <: AnyValue](x: Variable[Value], dx: Domain[Value]) = {
        val pruned = x.pruneDomain(dx)
        if (pruned) {
            val xs = new mutable.HashSet[AnyVariable]
            xs += x
            ReschedulePropagationStep(xs)
        } else {
            this
        }
    }
}

/**
  * Indicates that domains were pruned and that the propagation step needs rescheduling.
  *
  * @author Michael Marte
  */
case class ReschedulePropagationStep
    (override val affectedVariables: mutable.Set[AnyVariable])
    extends PropagationEffects
{
    require(! affectedVariables.isEmpty)
    override def rescheduleStep = true
    override def pruneDomain[Value <: AnyValue](x: Variable[Value], dx: Domain[Value]) = {
        val pruned = x.pruneDomain(dx)
        if (pruned) {
            affectedVariables += x
        }
        this
    }
}
