package yuck.core

/**
  * Provides an interface for working with integral value types.
  *
  * Values are immutable and hence we do not define += and similar operators.
  *
  * @author Michael Marte
  */
abstract class IntegralValue[Value] extends NumericalValue[Value] {
    def /(that: Value): Value
    def %(that: Value): Value
    def isEven: Boolean
}
