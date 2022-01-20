package yuck.core

/**
 * Provides an interface for working with ordered value types.
 *
 * @author Michael Marte
 */
abstract class OrderedValue[V] extends AnyValue with Ordered[V] {

    override def equals(that: Any) = that match {
        case rhs: AnyValue => {
            val lhs = this
            lhs.eq(rhs) || (lhs.valueType == rhs.valueType && lhs.compare(rhs.asInstanceOf[V]) == 0)
        }
        case _ => false
    }

    @inline def ==(that: OrderedValue[V]): Boolean = this.eq(that) || this.compare(that.asInstanceOf[V]) == 0
    @inline def !=(that: OrderedValue[V]): Boolean = ! ( this == that)

}
