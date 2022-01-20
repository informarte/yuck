package yuck.core

/**
 * Provides an interface for working with numerical value types.
 *
 * Values are immutable and hence we do not define += and similar operators.
 *
 * @author Michael Marte
 */
abstract class NumericalValue[V] extends OrderedValue[V] {
    def +(that: V): V
    def -(that: V): V
    def *(that: V): V
    /** Computes this + a - b. */
    def addAndSub(a: V, b: V): V
    /** Computes this + s * a - s * b. */
    def addAndSub(s: V, a: V, b: V): V
    def ^(that: V): V
    def abs: V
    def negate: V
    def toInt: Int
    def toLong: Long
    def toFloat: Float
    def toDouble: Double
}
