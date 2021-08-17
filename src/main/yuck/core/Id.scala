package yuck.core

/**
 * Represents an id for an object of the given type.
 *
 * To prevent inadvertent boxing, Id[T] does not implement Ordered[Id[T]].
 *
 * @author Michael Marte
 */
final class Id[T](val rawId: Int) extends AnyVal

/**
 * Companion object to Id[T].
 *
 * @author Michael Marte
 */
object Id {

    implicit def idOrdering[T]: Ordering[Id[T]] = new Ordering[Id[T]] {
        override def compare(lhs: Id[T], rhs: Id[T]) = lhs.rawId - rhs.rawId
    }

}
