package yuck.core

/**
 * Represents an id for an object of the given type.
 *
 * To prevent inadvertent boxing, Id[T] does not implement Ordered[Id[T]].
 *
 * @author Michael Marte
 */
final class Id[T](val rawId: Int) extends AnyVal {
    override def toString = rawId.toString
    @inline def compare(that: Id[T]) = this.rawId - that.rawId
}

/**
 * Companion object to Id[T].
 *
 * @author Michael Marte
 */
object Id {

    implicit def idOrdering[T] = new Ordering[Id[T]] {
        override def compare(lhs: Id[T], rhs: Id[T]) =
            lhs.compare(rhs)
    }

}
