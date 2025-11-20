package yuck.core

/**
 * Provides an interface for working with ordered value types.
 */
abstract class OrderedValue[V] extends Value[V] with Ordered[V]
