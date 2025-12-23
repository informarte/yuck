package yuck.core

/**
  * Provides an interface for working with integral value types.
  *
  * Values are immutable and hence we do not define += and similar operators.
  */
abstract class IntegralValue[A] extends NumericalValue[A] {
    def /(that: A): A
    def %(that: A): A
    def isEven: Boolean
}
