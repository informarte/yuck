package yuck.core

/**
 * Provides an interface for working with domains of unknown type.
 *
 * Domains are immutable and may be infinite.
 *
 * @author Michael Marte
 */
abstract class AnyDomain {

    /** Returns the type of the domain's elements. */
    def valueType: Class[_]

    /** Returns the domain's cardinality. */
    def size: Int

    /** Returns true iff the domain contains all values of the domain's type. */
    def isComplete: Boolean

    /** Returns true iff the domain is finite. */
    def isFinite: Boolean

    /** Returns true iff the domain is empty. */
    def isEmpty: Boolean = isFinite && size == 0

    /** Returns true iff the domain has one element. */
    def isSingleton: Boolean = isFinite && size == 1

    /**
     * Returns all values of the domain.
     *
     * Throws when the domain is infinite.
     */
    def values: Iterable[_ <: AnyValue]

    /**
      * Returns all values of the domain.
      *
      * Throws when the domain is infinite.
      */
    def valuesIterator: Iterator[_ <: AnyValue]

    /** Returns the domain's value when the domain is singleton and throws otherwise. */
    def singleValue: AnyValue

}
