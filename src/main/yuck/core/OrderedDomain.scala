package yuck.core

/**
 * Provides an interface for working with ordered, typed domains.
 *
 * Empty domains have lower and upper bounds; when a domain is empty,
 * its lower bound will be greater than its upper bound.
 *
 * Null bounds imply infinity.
 */
abstract class OrderedDomain[A <: OrderedValue[A], D <: OrderedDomain[A, D]] extends Domain[A, D] with Ordered[D] {

    /** Returns true iff the domain has a lower or an upper bound. */
    def isBounded: Boolean

    /** Returns true iff the domain has a lower bound. */
    def hasLb: Boolean = lb.ne(null)

    /** Returns true iff the domain has an upper bound. */
    def hasUb: Boolean = ub.ne(null)

    /** Provides the domain's lower bound as Option instance. */
    def maybeLb: Option[A] = Option(lb)

    /** Provides the domain's upper bound as Option instance. */
    def maybeUb: Option[A] = Option(ub)

    /** Returns the domain's lower bound when it exists and null otherwise. */
    def lb: A = maybeLb.getOrElse(null.asInstanceOf[A])

    /** Returns the domain's upper bound when it exists and null otherwise. */
    def ub: A = maybeUb.getOrElse(null.asInstanceOf[A])

    /** Returns [lb, ub]. */
    def hull: D

}
