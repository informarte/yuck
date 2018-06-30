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

    /** Basis for implementing equality constraints. */
    def eqc(that: Value): BooleanValue = if (this == that) True else False

    /** Basis for implementing inequality constraints. */
    def nec(that: Value): BooleanValue = if (this != that) True else False

    /** Basis for implementing ordering constraints. */
    def ltc(that: Value): BooleanValue = if (this < that) True else False

    /** Basis for implementing ordering constraints. */
    def lec(that: Value): BooleanValue = if (this <= that) True else False

}
