package yuck.core

/**
 * Provides an interface for working with values of known type.
 *
 * @author Michael Marte
 */
abstract class Value[V] extends AnyValue {

    override def equals(that: Any) = that match {
        case rhs: AnyValue => {
            val lhs = this
            lhs.eq(rhs) || (lhs.valueType == rhs.valueType && lhs == rhs.asInstanceOf[Value[V]])
        }
        case _ => false
    }

    def ==(that: Value[V]): Boolean
    inline final def !=(that: Value[V]): Boolean = ! (this == that)

}
