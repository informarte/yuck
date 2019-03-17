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
    /** Computes this + a - b. */
    def addAndSub(a: Value, b: Value): Value
    /** Computes this + s * a - s * b. */
    def addAndSub(s: Value, a: Value, b: Value): Value
    def /(that: Value): Value
    def ^(that: Value): Value
    def %(that: Value): Value
    def abs: Value
    def neg: Value
    def toInt: Int
    def toLong: Long
    def toDouble: Double
    def isEven: Boolean
}
