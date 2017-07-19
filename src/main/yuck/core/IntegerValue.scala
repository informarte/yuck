package yuck.core

/**
 * Implements immutable integer values.
 *
 * @author Michael Marte
 */
final class IntegerValue(val value: Int) extends NumericalValue[IntegerValue] {
    @inline override def hashCode = value
    override def equals(that: Any) = that match {
        case rhs: IntegerValue => {
            val lhs = this
            lhs.value == rhs.value
        }
        case _ => false
    }
    @inline override def compare(that: IntegerValue) = this.value - that.value
    override def toString = value.toString
    override def +(that: IntegerValue) = IntegerValue.get(this.value + that.value)
    override def -(that: IntegerValue) = IntegerValue.get(this.value - that.value)
    override def *(that: IntegerValue) = IntegerValue.get(this.value * that.value)
    override def /(that: IntegerValue) = IntegerValue.get(this.value / that.value)
    override def ^(that: IntegerValue) = IntegerValue.get(scala.math.pow(this.value, that.value).toInt)
    override def %(that: IntegerValue) = IntegerValue.get(this.value % that.value)
    override def abs = if (value < 0) IntegerValue.get(-value) else this
    override def toDouble = value.toDouble
    override def isEven = value % 2 == 0
}

/**
 * Provides properties of and operations on integer values.
 *
 * @author Michael Marte
 */
final object IntegerValue {

    def min(a: IntegerValue, b: IntegerValue): IntegerValue = if (a < b) a else b

    def max(a: IntegerValue, b: IntegerValue): IntegerValue = if (a > b) a else b

    implicit def valueTraits = IntegerValueTraits

    private val VALUE_RANGE = new Range(-10000, 10000, 1)
    private val valueCache = VALUE_RANGE.map(new IntegerValue(_)).toArray

    /**
     * Returns an IntegerValue instance for the given integer.
     *
     * Values in VALUE_RANGE are used as index into an array of prefabricated
     * IntegerValue instances, so the operation is cheap for them.
     * For other values, a new IntegerValue instance is created.
     */
    def get(a: Int): IntegerValue =
       if (VALUE_RANGE.contains(a)) valueCache.apply(a - VALUE_RANGE.start) else new IntegerValue(a)

}

/**
 * Provides traits of integer values.
 *
 * @author Michael Marte
 */
final object IntegerValueTraits extends NumericalValueTraits[IntegerValue] {
    override val valueType = classOf[IntegerValue]
    @inline override def compare(x: IntegerValue, y: IntegerValue) = x.compare(y)
    override val unboundedDomain = UnboundedIntegerDomain
    override val nonNegativeDomain = NonNegativeIntegerDomain
    override val zero = Zero
    override val one = One
    override def isSubsetOf(lhs: Domain[IntegerValue], rhs: Domain[IntegerValue]) =
        (lhs, rhs) match {
            case (lhs: IntegerRange, rhs: IntegerRange) =>
                lhs.isSubsetOf(rhs)
            case (lhs: IntegerDomain, rhs: IntegerDomain) =>
                lhs.isSubsetOf(rhs)
            case (lhs: IntegerRange, rhs: IntegerDomain) =>
                new IntegerDomain(Vector(lhs)).isSubsetOf(rhs)
            case (lhs: IntegerDomain, rhs: IntegerRange) =>
                lhs.isSubsetOf(new IntegerDomain(Vector(rhs)))
        }
}
