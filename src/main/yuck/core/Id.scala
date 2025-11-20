package yuck.core

/**
 * Represents an id for an object of the given type.
 *
 * To prevent inadvertent boxing, `Id[T]` does not implement `Ordered[Id[T]]`.
 */
final class Id[T](val rawId: Int) extends AnyVal

object Id {

    given ordering[T]: Ordering[Id[T]] with {
        override def compare(lhs: Id[T], rhs: Id[T]) = lhs.rawId - rhs.rawId
    }

}
