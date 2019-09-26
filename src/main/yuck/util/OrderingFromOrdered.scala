package yuck.util

import scala.language.existentials

/**
 * @author Michael Marte
 *
 */
class OrderingFromOrdered[T <: U forSome {type U <: Ordered[U]}] extends Ordering[T] {
    override def compare(a: T, b: T) = a.compare(b)
}
