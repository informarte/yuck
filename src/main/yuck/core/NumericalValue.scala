package yuck.core

/**
 * Provides an interface for working with numerical value types.
 *
 * Values are immutable and hence we do not define += and similar operators.
 */
abstract class NumericalValue[A] extends OrderedValue[A] {
    def +(that: A): A
    def -(that: A): A
    def *(that: A): A
    /** Computes this + a - b. */
    def addAndSub(a: A, b: A): A
    /** Computes this + s * a - s * b. */
    def addAndSub(s: A, a: A, b: A): A
    def ^(that: A): A
    def abs: A
    def negated: A
    def toInt: Int
    def toLong: Long
    def toFloat: Float
    def toDouble: Double
}
