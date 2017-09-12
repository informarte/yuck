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
    def ^(that: Value): Value
    def %(that: Value): Value
    def abs: Value
    def toDouble: Double
    def isEven: Boolean
}
