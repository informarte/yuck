package yuck.core

import scala.collection.*

/**
  * The effects of a propagation step.
  *
  * Remembers the variables the domains of which were pruned and knows whether the step needs rescheduling.
  *
  * Don't prune domains directly; instead use the pruning methods provided by this class!
  */
abstract class PropagationEffects {

    /** The variables the domains of which were pruned. */
    val affectedVariables: Iterable[AnyVariable]

    /** Whether the step needs rescheduling. */
    def rescheduleStep: Boolean

    /** Prunes the domain of the given variable and records the event. */
    def pruneDomain[A <: Value[A], D <: Domain[A, D], X <: Variable[A, D, X]](x: X, dx: D): PropagationEffects

    /** Prunes the domains of the given variables and records the events. */
    def pruneDomains
        [A <: Value[A], D <: Domain[A, D], X <: Variable[A, D, X],
         B <: Value[B], E <: Domain[B, E], Y <: Variable[B, E, Y]]
        (x: X, dx: D, y: Y, dy: E):
        PropagationEffects =
    {
        pruneDomain(x, dx).pruneDomain(y, dy)
    }

    /** Prunes the domains of the given variables and records the events. */
    def pruneDomains
        [A <: Value[A], D <: Domain[A, D], X <: Variable[A, D, X],
         B <: Value[B], E <: Domain[B, E], Y <: Variable[B, E, Y],
         C <: Value[C], F <: Domain[C, F], Z <: Variable[C, F, Z]]
        (x: X, dx: D, y: Y, dy: E, z: Z, dz: F):
        PropagationEffects =
    {
        pruneDomain(x, dx).pruneDomain(y, dy).pruneDomain(z, dz)
    }

    /** Prunes the domains of the given variables and records the events. */
    def pruneDomains
        [A <: Value[A], D <: Domain[A, D], X <: Variable[A, D, X]]
        (xds: Iterator[(X, D)]):
        PropagationEffects =
    {
        xds.foldLeft(this){case (result, (x, dx)) => result.pruneDomain(x, dx)}
    }

    /** Prunes the domains of the given variables and records the events. */
    def pruneDomains
        [A <: Value[A], D <: Domain[A, D], X <: Variable[A, D, X]]
        (xds: Iterable[(X, D)]):
        PropagationEffects =
    {
        xds.foldLeft(this){case (result, (x, dx)) => result.pruneDomain(x, dx)}
    }

}

/**
  * Starting point for constraint propagation.
  */
case object NoPropagationOccurred extends PropagationEffects {
    override val affectedVariables = Nil
    override def rescheduleStep = false
    override def pruneDomain[A <: Value[A], D <: Domain[A, D], X <: Variable[A, D, X]](x: X, dx: D) = {
        val pruned = x.pruneDomain(dx)
        if pruned then {
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
  */
final case class ReschedulePropagationStep
    (override val affectedVariables: mutable.Set[AnyVariable])
    extends PropagationEffects
{
    require(! affectedVariables.isEmpty)
    override def rescheduleStep = true
    override def pruneDomain[A <: Value[A], D <: Domain[A, D], X <: Variable[A, D, X]](x: X, dx: D) = {
        val pruned = x.pruneDomain(dx)
        if pruned then {
            affectedVariables += x
        }
        this
    }
}
