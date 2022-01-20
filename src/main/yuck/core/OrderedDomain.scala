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
abstract class OrderedDomain[V <: OrderedValue[V]] extends Domain[V] with Ordered[OrderedDomain[V]] {

    /** Returns true iff the domain has a lower or an upper bound. */
    def isBounded: Boolean

    /** Returns true iff the domain has a lower bound. */
    def hasLb: Boolean = lb.ne(null)

    /** Returns true iff the domain has an upper bound. */
    def hasUb: Boolean = ub.ne(null)

    /** Provides the domain's lower bound as Option instance. */
    def maybeLb: Option[V] = Option(lb)

    /** Provides the domain's upper bound as Option instance. */
    def maybeUb: Option[V] = Option(ub)

    /** Returns the domain's lower bound when it exists and null otherwise. */
    def lb: V = maybeLb.getOrElse(null.asInstanceOf[V])

    /** Returns the domain's upper bound when it exists and null otherwise. */
    def ub: V = maybeUb.getOrElse(null.asInstanceOf[V])

    /** Returns [lb, ub]. */
    def hull: OrderedDomain[V]

    override def randomSubdomain(randomGenerator: RandomGenerator): OrderedDomain[V]

    override def intersect(that: Domain[V]): OrderedDomain[V]
    override def union(that: Domain[V]): OrderedDomain[V]
    override def diff(that: Domain[V]): OrderedDomain[V]
    override def symdiff(that: Domain[V]): OrderedDomain[V] =
        super.symdiff(that).asInstanceOf[OrderedDomain[V]]

}
