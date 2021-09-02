package yuck.util

/**
 * @author Michael Marte
 *
 */
class OrderingFromOrdered[T <: Ordered[T]] extends Ordering[T] {
    override def compare(a: T, b: T) = a.compare(b)
}
