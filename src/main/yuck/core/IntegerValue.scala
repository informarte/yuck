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
    @inline override def compare(that: IntegerValue) = safeSub(this.value, that.value)
    override def toString = value.toString
    override def +(that: IntegerValue) = IntegerValue.get(safeAdd(this.value, that.value))
    override def -(that: IntegerValue) = IntegerValue.get(safeSub(this.value, that.value))
    override def *(that: IntegerValue) = IntegerValue.get(safeMul(this.value, that.value))
    override def /(that: IntegerValue) = IntegerValue.get(this.value / that.value)
    override def ^(that: IntegerValue) = {
        val result: Double = scala.math.pow(this.value, that.value)
        if (result == java.lang.Double.NEGATIVE_INFINITY || result == java.lang.Double.POSITIVE_INFINITY ||
            result < Int.MinValue || result > Int.MaxValue)
        {
            throw new java.lang.ArithmeticException("integer overflow")
        }
        IntegerValue.get(result.toInt)
    }
    override def %(that: IntegerValue) = IntegerValue.get(this.value % that.value)
    override def addAndSub(s: IntegerValue, a: IntegerValue, b: IntegerValue) =
        IntegerValue.get(safeAdd(this.value, safeMul(s.value, safeSub(a.value, b.value))))
    override def abs = if (value < 0) IntegerValue.get(safeNeg(value)) else this
    override def toInt = value
    override def toDouble = value.toDouble
    override def isEven = value % 2 == 0
    override def eqc(that: IntegerValue) = BooleanValue.get(scala.math.abs(safeSub(this.value, that.value)))
    override def nec(that: IntegerValue) = if (this != that) True else False
    override def ltc(that: IntegerValue) = if (this < that) True else BooleanValue.get(safeInc(safeSub(this.value, that.value)))
    override def lec(that: IntegerValue) = if (this <= that) True else BooleanValue.get(safeSub(this.value, that.value))
}

/**
 * Companion object to IntegerValue.
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
