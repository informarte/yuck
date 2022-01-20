package yuck.core

/**
  * Provides an interface for working with integral value types.
  *
  * Values are immutable and hence we do not define += and similar operators.
  *
  * @author Michael Marte
  */
abstract class IntegralValue[V] extends NumericalValue[V] {
    def /(that: V): V
    def %(that: V): V
    def isEven: Boolean
}
