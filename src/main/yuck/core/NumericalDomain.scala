package yuck.core

/**
 * Provides an interface for working with numerical domains.
 *
 * @author Michael Marte
 */
abstract class NumericalDomain[V <: NumericalValue[V]] extends OrderedDomain[V] {

    /**
     * Returns a domain that contains all elements of this domain except for those
     * smaller than the given lower bound.
     */
    def boundFromBelow(lb: V): NumericalDomain[V]

    /**
     * Returns a domain that contains all elements of this domain except for those
     * greater than the given upper bound.
     */
    def boundFromAbove(ub: V): NumericalDomain[V]

    /**
     * Returns a bisection of this domain.
     *
     * Throws when this domain is empty or infinite.
     */
    def bisect: (NumericalDomain[V], NumericalDomain[V])

    override def hull: NumericalDomain[V]

    /**
     * Negates all values of this domain and returns the domain created from the resulting values.
     */
    def mirrored: NumericalDomain[V]

    /**
     * Returns 0 if the domain contains the given value;
     * otherwise returns the distance of the given value to the nearest range.
     *
     * Throws when the domain is empty.
     */
    def distanceTo(a: V): V

    override def randomSubdomain(randomGenerator: RandomGenerator): NumericalDomain[V]
    override def intersect(that: Domain[V]): NumericalDomain[V]
    override def union(that: Domain[V]): NumericalDomain[V]
    override def diff(that: Domain[V]): NumericalDomain[V]
    override def symdiff(that: Domain[V]): NumericalDomain[V] =
        super.symdiff(that).asInstanceOf[NumericalDomain[V]]

}
