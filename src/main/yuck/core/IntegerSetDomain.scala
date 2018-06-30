package yuck.core

/**
 * Provides an interface for working with domains that contain integer
 * sets as elements.
 *
 * @author Michael Marte
 */
abstract class IntegerSetDomain extends OrderedDomain[IntegerSetValue] {
    final override def valueTraits = IntegerSetValueTraits
    final override def equals(that: Domain[IntegerSetValue]): Boolean = (this, that) match {
        case (lhs: SingletonIntegerSetDomain, rhs: SingletonIntegerSetDomain) => lhs.equals(rhs)
        case (lhs: SingletonIntegerSetDomain, rhs: IntegerPowersetDomain) => lhs.base.isEmpty && rhs.base.isEmpty
        case (lhs: IntegerPowersetDomain, rhs: IntegerPowersetDomain) => lhs.equals(rhs)
        case (lhs: IntegerPowersetDomain, rhs: SingletonIntegerSetDomain) => lhs.base.isEmpty && rhs.base.isEmpty
        case _ => ???
    }
    final override def hasLb = true
    final override def hasUb = true
    final override def compare(that: Domain[IntegerSetValue]): Int =
        this.compare(that.asInstanceOf[IntegerSetDomain])
    @inline def compare(that: IntegerSetDomain): Int = IntegerSetDomain.ordering.compare(this, that)
    final override def isSubsetOf(that: Domain[IntegerSetValue]): Boolean = (this, that) match {
        case (lhs: SingletonIntegerSetDomain, rhs: SingletonIntegerSetDomain) => lhs.isSubsetOf(rhs)
        case (lhs: SingletonIntegerSetDomain, rhs: IntegerPowersetDomain) => lhs.base.isSubsetOf(rhs.base)
        case (lhs: IntegerPowersetDomain, rhs: IntegerPowersetDomain) => lhs.isSubsetOf(rhs)
        case (lhs: IntegerPowersetDomain, rhs: SingletonIntegerSetDomain) => lhs.base.isEmpty && rhs.base.isEmpty
        case _ => ???
    }
    final override def intersects(that: Domain[IntegerSetValue]): Boolean = (this, that) match {
        case (lhs: SingletonIntegerSetDomain, rhs: SingletonIntegerSetDomain) => lhs.intersects(rhs)
        case (lhs: SingletonIntegerSetDomain, rhs: IntegerPowersetDomain) => lhs.base.isSubsetOf(rhs.base)
        case (lhs: IntegerPowersetDomain, rhs: IntegerPowersetDomain) => lhs.intersects(rhs)
        case (lhs: IntegerPowersetDomain, rhs: SingletonIntegerSetDomain) => rhs.base.isSubsetOf(lhs.base)
        case _ => ???
    }
    final override def intersect(that: Domain[IntegerSetValue]): IntegerSetDomain = (this, that) match {
        case (lhs: IntegerPowersetDomain, rhs: IntegerPowersetDomain) => lhs.intersect(rhs)
        case _ => ??? // result cannot be represented
    }
    final override def union(that: Domain[IntegerSetValue]): IntegerSetDomain = (this, that) match {
        case (lhs: IntegerPowersetDomain, rhs: IntegerPowersetDomain) => lhs.union(rhs)
        case _ => ??? // result cannot be represented
    }
    final override def diff(that: Domain[IntegerSetValue]): IntegerSetDomain = (this, that) match {
        case _ => ??? // result cannot be represented
    }
}

/**
 * Companion object to IntegerSetDomain.
 *
 * @author Michael Marte
 */
final object IntegerSetDomain {

    private def lessThan(lhs: IntegerSetDomain, rhs: IntegerSetDomain) =
        lhs.lb < rhs.lb || (lhs.lb == rhs.lb && lhs.ub < rhs.ub)

    /** A lexicographic ordering on the boundaries. */
    // (There is currently no implementation of IntegerSetDomain that allows for holes.)
    val ordering = Ordering.fromLessThan(lessThan)

}
