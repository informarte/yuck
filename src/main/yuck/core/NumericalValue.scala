package yuck.core

/**
 * Provides an interface for working with numerical value types.
 *
 * Values are immutable and hence we do not define += and similar operators.
 *
 * @author Michael Marte
 */
abstract class NumericalValue[Value] extends OrderedValue[Value] {
    def +(that: Value): Value
    def -(that: Value): Value
    def *(that: Value): Value
    def /(that: Value): Value
    def %(that: Value): Value
    def abs: Value
    def toDouble: Double
    def isEven: Boolean
}

/**
 * Provides properties of numerical values.
 *
 * @author Michael Marte
 */
trait NumericalValueTraits[Value <: NumericalValue[Value]] extends OrderedValueTraits[Value] {

    final def staticCast(x: OrderedDomain[Value]): NumericalDomain[Value] =
        x.asInstanceOf[NumericalDomain[Value]]

    /** Returns the additive identity. */
    val zero: Value

    /** Returns the multiplicative identity. */
    val one: Value

    /** Returns the domain that contains all non-negative values. */
    val nonNegativeDomain: Domain[Value]

}
