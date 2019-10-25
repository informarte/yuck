package yuck.core

/**
 * Provides an interface for working with ordered, typed domains.
 *
 * Empty domains have lower and upper bounds; when a domain is empty,
 * its lower bound will be greater than its upper bound.
 *
 * Null bounds imply infinity.
 *
 * @author Michael Marte
 */
abstract class OrderedDomain[Value <: OrderedValue[Value]] extends Domain[Value] with Ordered[OrderedDomain[Value]] {

    /** Returns true iff the domain has a lower or an upper bound. */
    def isBounded: Boolean

    /** Returns true iff the domain has a lower bound. */
    def hasLb: Boolean = lb != null

    /** Returns true iff the domain has an upper bound. */
    def hasUb: Boolean = ub != null

    /** Provides the domain's lower bound as Option instance. */
    def maybeLb: Option[Value] = Option(lb)

    /** Provides the domain's upper bound as Option instance. */
    def maybeUb: Option[Value] = Option(ub)

    /** Returns the domain's lower bound when it exists and null otherwise. */
    def lb: Value = maybeLb.getOrElse(null.asInstanceOf[Value])

    /** Returns the domain's upper bound when it exists and null otherwise. */
    def ub: Value = maybeUb.getOrElse(null.asInstanceOf[Value])

    /** Returns [lb, ub]. */
    def hull: OrderedDomain[Value]

    override def randomSubdomain(randomGenerator: RandomGenerator): OrderedDomain[Value]
    override def intersect(that: Domain[Value]): OrderedDomain[Value]
    override def union(that: Domain[Value]): OrderedDomain[Value]
    override def diff(that: Domain[Value]): OrderedDomain[Value]
    override def symdiff(that: Domain[Value]): OrderedDomain[Value] =
        super.symdiff(that).asInstanceOf[OrderedDomain[Value]]

}
