package yuck.core

/**
 * Provides an interface for working with ordered value types.
 */
abstract class OrderedValue[A] extends Value[A] with Ordered[A]
