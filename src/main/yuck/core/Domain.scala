package yuck.core

/**
 * Provides an interface for working with typed domains.
 *
 * @author Michael Marte
 */
abstract class Domain[Value <: AnyValue] extends AnyDomain {

    override def equals(that: Any) = that match {
        case rhs: Domain[_] => {
            val lhs = this
            lhs.eq(rhs) ||
            (lhs.valueType == rhs.valueType && lhs.equals(rhs.asInstanceOf[Domain[Value]]))
        }
        case _ => false
    }
    def equals(that: Domain[Value]): Boolean

    override def toString = "{%s}".format(valuesIterator.map(_.toString).mkString(", "))

    override def values: Iterable[Value]
    override def valuesIterator: Iterator[Value] = values.iterator
    override def singleValue: Value

    /** Decides whether the domain contains the given value. */
    def contains(a: Value): Boolean

    /**
     * Returns a random value from the domain.
     *
     * Throws when the domain is empty or infinite.
     */
    def randomValue(randomGenerator: RandomGenerator): Value

    /**
     * Returns a random value from the domain.
     *
     * If the domain has at least two elements, the return value is guaranteed to differ
     * from the given value.
     *
     * Throws when the domain is empty or infinite.
     */
    def nextRandomValue(randomGenerator: RandomGenerator, currentValue: Value): Value

    /**
     * Chooses a random subdomain from the domain.
     *
     * Throws when the domain is infinite.
     */
    def randomSubdomain(randomGenerator: RandomGenerator): Domain[Value]

    /** Decides whether this is a subset of that. */
    def isSubsetOf(that: Domain[Value]): Boolean

    /** Decides whether this intersects that. */
    def intersects(that: Domain[Value]): Boolean

    /** Computes the intersection of this and that. */
    def intersect(that: Domain[Value]): Domain[Value]

    /** Computes the union of this and that. */
    def union(that: Domain[Value]): Domain[Value]

    /** Computes this \ that. */
    def diff(that: Domain[Value]): Domain[Value]

    /** Computes the symmetrical difference of this and that. */
    def symdiff(that: Domain[Value]): Domain[Value] = this.union(that).diff(this.intersect(that))

}
