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
            lhs.eq(rhs) ||
            (lhs.valueType == rhs.valueType && lhs.compare(rhs.asInstanceOf[Value]) == 0)
        }
        case _ => false
    }
}

/**
 * Provides properties of ordered values.
 *
 * @author Michael Marte
 */
trait OrderedValueTraits[Value <: OrderedValue[Value]] extends AnyValueTraits[Value] with Ordering[Value] {
}
