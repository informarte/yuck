package yuck.core

/**
 * Provides an interface for working with numerical domains.
 */
abstract class NumericalDomain[A <: NumericalValue[A], D <: NumericalDomain[A, D]] extends OrderedDomain[A, D] {

    /**
     * Returns a domain that contains all elements of this domain except for those
     * smaller than the given lower bound.
     */
    def boundFromBelow(lb: A): D

    /**
     * Returns a domain that contains all elements of this domain except for those
     * greater than the given upper bound.
     */
    def boundFromAbove(ub: A): D

    /**
     * Returns a bisection of this domain.
     *
     * Throws when this domain is empty or infinite.
     */
    def bisect: (D, D)

    /**
     * Negates all values of this domain and returns the domain created from the resulting values.
     */
    def mirrored: D

    /**
     * Returns 0 if the domain contains the given value;
     * otherwise returns the distance of the given value to the nearest range.
     *
     * Throws when the domain is empty.
     */
    def distanceTo(a: A): A

}
