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

    /**
     * Negates all values of this domain and returns the domain created from the resulting values.
     */
    def mirrored: NumericalDomain[Value]

    /**
     * Returns 0 if the domain contains the given value;
     * otherwise returns the distance of the given value to the nearest range.
     *
     * Throws when the domain is empty.
     */
    def distanceTo(a: NumericalValue[Value]): NumericalValue[Value]

    override def randomSubdomain(randomGenerator: RandomGenerator): NumericalDomain[Value]
    override def intersect(that: Domain[Value]): NumericalDomain[Value]
    override def union(that: Domain[Value]): NumericalDomain[Value]
    override def diff(that: Domain[Value]): NumericalDomain[Value]
    override def symdiff(that: Domain[Value]): NumericalDomain[Value] =
        super.symdiff(that).asInstanceOf[NumericalDomain[Value]]

}
