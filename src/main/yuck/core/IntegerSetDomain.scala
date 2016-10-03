package yuck.core

/**
 * Provides an interface for working with domains that contain integer
 * sets as elements.
 *
 * @author Michael Marte
 */
abstract class IntegerSetDomain extends OrderedDomain[IntegerSetValue] {
    final override def valueTraits = IntegerSetValue.Traits
    val base: IntegerDomain
    final override def hashCode = base.hashCode
    final override def equals(that: Any) = that match {
        case rhs: IntegerSetDomain => {
            val lhs = this
            lhs.eq(rhs) || lhs.base.eq(rhs.base) || lhs.base == rhs.base
        }
        case _ => false
    }
}
