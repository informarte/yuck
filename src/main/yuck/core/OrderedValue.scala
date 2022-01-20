package yuck.core

/**
 * Provides an interface for working with ordered value types.
 *
 * @author Michael Marte
 */
abstract class OrderedValue[V] extends Value[V] with Ordered[V]
