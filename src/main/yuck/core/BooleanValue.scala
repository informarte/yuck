package yuck.core

/**
 * Implements immutable Boolean values.
 *
 * @author Michael Marte
 */
final class BooleanValue(val value: Boolean) extends OrderedValue[BooleanValue] {
    @inline override def hashCode = if (value) 1 else 0
    override def equals(that: Any) = that match {
        case rhs: BooleanValue => {
            val lhs = this
            lhs.value == rhs.value
        }
        case _ => false
    }
    @inline override def compare(that: BooleanValue) =
        if (this.value == that.value) 0 else if (this.value) 1 else - 1
    override def toString = value.toString
    /** Implements negation. */
    def not: BooleanValue = if (value) False else True
}

/**
 * Provides operations on Boolean values.
 *
 * @author Michael Marte
 */
final object BooleanValue {

    implicit def valueTraits = BooleanValueTraits

}

/**
 * Provides traits of Boolean values.
 *
 * @author Michael Marte
 */
final object BooleanValueTraits extends OrderedValueTraits[BooleanValue] {
    override val valueType = classOf[BooleanValue]
    @inline override def compare(x: BooleanValue, y: BooleanValue) = x.compare(y)
    override val unboundedDomain = UnboundedBooleanDomain
}
