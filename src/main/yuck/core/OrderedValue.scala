package yuck.core

/**
 * Provides an interface for working with ordered value types.
 *
 * @author Michael Marte
 */
abstract class OrderedValue[Value] extends AnyValue with Ordered[Value] {

    override def equals(that: Any) = that match {
        case rhs: AnyValue => {
            val lhs = this
            lhs.eq(rhs) || (lhs.valueType == rhs.valueType && lhs.compare(rhs.asInstanceOf[Value]) == 0)
        }
        case _ => false
    }

    @inline def ==(that: OrderedValue[Value]): Boolean = this.eq(that) || this.compare(that.asInstanceOf[Value]) == 0
    @inline def !=(that: OrderedValue[Value]): Boolean = ! ( this == that)

}
