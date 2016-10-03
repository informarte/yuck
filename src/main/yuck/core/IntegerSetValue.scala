package yuck.core

/**
 * Implements immutable integer-set values.
 *
 * @author Michael Marte
 */
final class IntegerSetValue(val set: IntegerDomain) extends OrderedValue[IntegerSetValue] {
    @inline override def hashCode = set.hashCode
    override def equals(that: Any) = that match {
        case rhs: IntegerSetValue => {
            val lhs = this
            lhs.eq(rhs) || lhs.set.eq(rhs.set) || lhs.set.equals(rhs.set)
        }
        case _ => false
    }
    override def compare(that: IntegerSetValue) =
        if (this.set.equals(that.set)) 0
        else if (this.set.isSubsetOf(that.set)) -1
        else if (that.set.isSubsetOf(this.set)) +1
        else if (this.set.lb < that.set.lb) -1
        else +1
    override def toString = set.toString
}

/**
 * Provides properties of and operations on integer-set values.
 *
 * @author Michael Marte
 */
final object IntegerSetValue {

    final implicit object Traits extends OrderedValueTraits[IntegerSetValue] {
        override val valueType = classOf[IntegerSetValue]
        @inline override def compare(x: IntegerSetValue, y: IntegerSetValue) = x.compare(y)
        override val unboundedDomain = UnboundedIntegerSetDomain
        override def isSubsetOf(lhs: Domain[IntegerSetValue], rhs: Domain[IntegerSetValue]) =
            (lhs, rhs) match {
              case (lhs: SingletonIntegerSetDomain, rhs: SingletonIntegerSetDomain) =>
                  lhs.isSubsetOf(rhs)
              case (lhs: SingletonIntegerSetDomain, rhs: IntegerPowersetDomain) =>
                  lhs.base.isSubsetOf(rhs.base)
              case (lhs: IntegerPowersetDomain, rhs: IntegerPowersetDomain) =>
                  lhs.base.isSubsetOf(rhs.base)
              case (lhs: IntegerPowersetDomain, rhs: SingletonIntegerSetDomain) =>
                  false
        }
    }

}
