package yuck.core

/**
 * Represents an id for an object of the given type.
 *
 * @author Michael Marte
 */
final class Id[Type](val rawId: Int) extends Ordered[Id[Type]] {
    @inline override def hashCode = rawId
    override def toString = rawId.toString
    override def compare(that: Id[Type]) = this.rawId - that.rawId
    // equals cannot be implemented in a generic way due to type erasure but we
    // do not need it anyway because id objects cannot not be cloned.
}
