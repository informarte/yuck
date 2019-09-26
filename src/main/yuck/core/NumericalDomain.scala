package yuck.core

/**
 * Provides an interface for working with numerical domains.
 *
 * @author Michael Marte
 */
abstract class NumericalDomain[Value <: NumericalValue[Value]] extends OrderedDomain[Value] {

    /**
     * Returns a domain that contains all elements of this domain except for those
     * smaller than the given lower bound.
     */
    def boundFromBelow(lb: Value): NumericalDomain[Value]

    /**
     * Returns a domain that contains all elements of this domain except for those
     * greater than the given upper bound.
     */
    def boundFromAbove(lb: Value): NumericalDomain[Value]

    /**
     * Returns a bisection of this domain.
     *
     * Throws when this domain is empty or infinite.
     */
    def bisect: (NumericalDomain[Value], NumericalDomain[Value])

    override def hull: NumericalDomain[Value]

    override def intersect(that: Domain[Value]): NumericalDomain[Value]
    override def union(that: Domain[Value]): NumericalDomain[Value]
    override def diff(that: Domain[Value]): NumericalDomain[Value]
    override def symdiff(that: Domain[Value]): NumericalDomain[Value] =
        super.symdiff(that).asInstanceOf[NumericalDomain[Value]]

}
