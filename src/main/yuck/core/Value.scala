package yuck.core

/**
 * Provides an interface for working with values of known type.
 */
abstract class Value[A] extends AnyValue {

    override def equals(that: Any) = that match {
        case rhs: AnyValue =>
            val lhs = this
            lhs.eq(rhs) || (lhs.valueType == rhs.valueType && lhs == rhs.asInstanceOf[Value[A]])
        case _ => false
    }

    def ==(that: Value[A]): Boolean
    inline final def !=(that: Value[A]): Boolean = ! (this == that)

}
