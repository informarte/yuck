package yuck.core

import java.lang.Math.pow

/**
 * Implements immutable integer values.
 *
 * @author Michael Marte
 */
final class IntegerValue(val value: Int) extends IntegralValue[IntegerValue] {
    @inline override def hashCode = value
    override def equals(that: Any) = that match {
        case rhs: IntegerValue => {
            val lhs = this
            lhs.value == rhs.value
        }
        case _ => false
    }
    override def compare(that: IntegerValue) =
        if (this.value < that.value) -1
        else if (this.value > that.value) 1
        else 0
    override def ==(that: Value[IntegerValue]) = this == that.asInstanceOf[IntegerValue]
    @inline def ==(that: IntegerValue): Boolean = this.value == that.value
    @inline def !=(that: IntegerValue): Boolean = this.value != that.value
    @inline override def <(that: IntegerValue) = this.value < that.value
    @inline override def <=(that: IntegerValue) = this.value <= that.value
    @inline override def >(that: IntegerValue) = this.value > that.value
    @inline override def >=(that: IntegerValue) = this.value >= that.value
    override def toString = value.toString
    override def +(that: IntegerValue) =
        if (this.value == 0) that
        else if (that.value == 0) this
        else IntegerValue(safeAdd(this.value, that.value))
    override def -(that: IntegerValue) = if (that.value == 0) this else IntegerValue(safeSub(this.value, that.value))
    override def *(that: IntegerValue) = IntegerValue(safeMul(this.value, that.value))
    override def /(that: IntegerValue) = IntegerValue(this.value / that.value)
    override def %(that: IntegerValue) = IntegerValue(this.value % that.value)
    override def ^(that: IntegerValue) = {
        val result: Double = pow(this.value, that.value)
        if (result == java.lang.Double.NEGATIVE_INFINITY || result == java.lang.Double.POSITIVE_INFINITY ||
            result < Int.MinValue || result > Int.MaxValue)
        {
            throw new java.lang.ArithmeticException("integer overflow")
        }
        IntegerValue(result.toInt)
    }
    override def addAndSub(a: IntegerValue, b: IntegerValue) = {
        val delta = safeSub(a.value, b.value)
        if (delta == 0) this else IntegerValue(safeAdd(this.value, delta))
    }
    override def addAndSub(s: IntegerValue, a: IntegerValue, b: IntegerValue) = {
        val delta = safeSub(a.value, b.value)
        if (delta == 0) this else IntegerValue(safeAdd(this.value, safeMul(s.value, delta)))
    }
    @inline override def abs = if (value < 0) negate else this
    @inline override def negate = IntegerValue(safeNeg(value))
    @inline override def toInt = value
    @inline override def toLong = value.toLong
    @inline override def toFloat = value.toFloat
    @inline override def toDouble = value.toDouble
    @inline override def isEven = value % 2 == 0
}

/**
 * Companion object to IntegerValue.
 *
 * @author Michael Marte
 */
object IntegerValue {

    @inline def min(a: IntegerValue, b: IntegerValue): IntegerValue = if (a < b) a else b

    @inline def max(a: IntegerValue, b: IntegerValue): IntegerValue = if (a > b) a else b

    implicit def valueTraits: IntegerValueTraits.type = IntegerValueTraits
    implicit def numericalOperations: IntegerValueOperations.type = IntegerValueOperations
    implicit def domainOrdering: IntegerDomainOrdering.type = IntegerDomainOrdering

    private val lb = -10000
    private val ub = 10000
    private val valueRange = Range(lb, ub, 1)
    private val valueCache = valueRange.iterator.map(new IntegerValue(_)).toArray

    /**
     * Returns an IntegerValue instance for the given integer.
     *
     * Values in valueRange are used as index into an array of prefabricated
     * IntegerValue instances, so the operation is cheap for them.
     * For other values, a new IntegerValue instance is created.
     */
    def apply(a: Int): IntegerValue =
       if (lb <= a && a < ub) valueCache(a - lb) else new IntegerValue(a)

}
