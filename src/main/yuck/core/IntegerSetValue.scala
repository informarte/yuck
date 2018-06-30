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
    override def compare(that: IntegerSetValue) = this.set.compare(that.set)
    override def toString = set.toString
    override def eqc(that: IntegerSetValue) =
        BooleanValue.get(
            safeAdd(this.set.maybeResidueSize(that.set).getOrElse(1), that.set.maybeResidueSize(this.set).getOrElse(1)))
}

/**
 * Companion object to IntegerSetValue.
 *
 * @author Michael Marte
 */
final object IntegerSetValue {

    implicit def valueTraits = IntegerSetValueTraits

}
