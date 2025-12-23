package yuck.core

/**
 * Provides an interface for working with domains of known type.
 */
abstract class Domain[A <: Value[A], D <: Domain[A, D]] extends AnyDomain {

    override def equals(that: Any) = that match {
        case rhs: Domain[?, ?] =>
            val lhs = this
            lhs.eq(rhs) || (lhs.valueType == rhs.valueType && lhs.asInstanceOf[D] == rhs.asInstanceOf[D])
        case _ => false
    }

    def ==(that: D): Boolean
    inline final def !=(that: D): Boolean = ! ( this == that)

    override def toString = "{%s}".format(valuesIterator.map(_.toString).mkString(", "))

    override def values: Iterable[A]
    override def valuesIterator: Iterator[A] = values.iterator
    override def singleValue: A

    /** Decides whether the domain contains the given value. */
    def contains(a: A): Boolean

    /**
     * Returns a random value from the domain.
     *
     * Throws when the domain is empty or infinite.
     */
    def randomValue(randomGenerator: RandomGenerator): A

    /**
     * Returns a random value from the domain.
     *
     * If the domain has at least two elements, the return value is guaranteed to differ
     * from the given value.
     *
     * Throws when the domain is empty or infinite.
     */
    def nextRandomValue(randomGenerator: RandomGenerator, currentValue: A): A

    /**
     * Chooses a random subdomain from the domain.
     *
     * Throws when the domain is infinite.
     */
    def randomSubdomain(randomGenerator: RandomGenerator): D

    /** Decides whether this is a subset of that. */
    def isSubsetOf(that: D): Boolean

    /** Decides whether this intersects that. */
    def intersects(that: D): Boolean

    /** Computes the intersection of this and that. */
    def intersect(that: D): D

    /** Computes the union of this and that. */
    def union(that: D): D

    /** Computes this \ that. */
    def diff(that: D): D

    /** Computes the symmetrical difference of this and that. */
    def symdiff(that: D): D = this.union(that).diff(this.intersect(that))

}
