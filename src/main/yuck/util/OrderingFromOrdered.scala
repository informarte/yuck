package yuck.util

class OrderingFromOrdered[T <: Ordered[T]] extends Ordering[T] {
    inline override def compare(a: T, b: T) = a.compare(b)
}
