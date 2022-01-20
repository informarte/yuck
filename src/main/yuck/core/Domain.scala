package yuck.core

/**
 * Provides an interface for working with domains of known type.
 *
 * @author Michael Marte
 */
abstract class Domain[V <: AnyValue] extends AnyDomain {

    override def equals(that: Any) = that match {
        case rhs: Domain[_] => {
            val lhs = this
            lhs.eq(rhs) ||
            (lhs.valueType == rhs.valueType && lhs == rhs.asInstanceOf[Domain[V]])
        }
        case _ => false
    }

    def ==(that: Domain[V]): Boolean
    @inline final def !=(that: Domain[V]): Boolean = ! ( this == that)

    override def toString = "{%s}".format(valuesIterator.map(_.toString).mkString(", "))

    override def values: Iterable[V]
    override def valuesIterator: Iterator[V] = values.iterator
    override def singleValue: V

    /** Decides whether the domain contains the given value. */
    def contains(a: V): Boolean

    /**
     * Returns a random value from the domain.
     *
     * Throws when the domain is empty or infinite.
     */
    def randomValue(randomGenerator: RandomGenerator): V

    /**
     * Returns a random value from the domain.
     *
     * If the domain has at least two elements, the return value is guaranteed to differ
     * from the given value.
     *
     * Throws when the domain is empty or infinite.
     */
    def nextRandomValue(randomGenerator: RandomGenerator, currentValue: V): V

    /**
     * Chooses a random subdomain from the domain.
     *
     * Throws when the domain is infinite.
     */
    def randomSubdomain(randomGenerator: RandomGenerator): Domain[V]

    /** Decides whether this is a subset of that. */
    def isSubsetOf(that: Domain[V]): Boolean

    /** Decides whether this intersects that. */
    def intersects(that: Domain[V]): Boolean

    /** Computes the intersection of this and that. */
    def intersect(that: Domain[V]): Domain[V]

    /** Computes the union of this and that. */
    def union(that: Domain[V]): Domain[V]

    /** Computes this \ that. */
    def diff(that: Domain[V]): Domain[V]

    /** Computes the symmetrical difference of this and that. */
    def symdiff(that: Domain[V]): Domain[V] = this.union(that).diff(this.intersect(that))

}
