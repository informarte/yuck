package yuck.core

/**
 * Implements immutable Boolean values with non-negative violations.
 *
 * A zero violation means "true", a non-zero violation means "false" to a certain degree.
 *
 * Values are compared by violation and hence:
 *
 *  - The smaller the violation, the smaller the value.
 *    In particular, "true" is smaller than all "false" values.
 *  - "false" values with different violations are different and therefore direct comparison
 *    is discouraged; use truthValue instead, it's safer and more efficient than equals!
 *
 * @author Michael Marte
 */
final class BooleanValue(val violation: Int) extends NumericalValue[BooleanValue] {
    require(violation >= 0)
    def this(value: Boolean) = this(if (value) 0 else 1)
    @inline override def hashCode = violation.hashCode
    override def equals(that: Any) = that match {
        case rhs: BooleanValue => {
            val lhs = this
            lhs.violation == rhs.violation
        }
        case _ => false
    }
    @inline override def compare(that: BooleanValue) = this.violation.compare(that.violation)
    override def toString =
        if (violation == 0) "true" else if (violation == 1) "false" else "false(%d)".format(violation)
    /** Returns true iff the violation is zero. */
    @inline def truthValue: Boolean = violation == 0
    /** Implements negation. */
    def not: BooleanValue = if (truthValue) False else True
    override def +(that: BooleanValue) = BooleanValue.get(safeAdd(this.violation, that.violation))
    override def -(that: BooleanValue) = BooleanValue.get(safeSub(this.violation, that.violation))
    override def *(that: BooleanValue) = BooleanValue.get(safeMul(this.violation, that.violation))
    override def /(that: BooleanValue) = ???
    override def ^(that: BooleanValue) = ???
    override def %(that: BooleanValue) = ???
    override def addAndSub(s: BooleanValue, a: BooleanValue, b: BooleanValue) =
        BooleanValue.get(
            safeSub(
                safeAdd(this.violation, safeMul(s.violation, a.violation)),
                safeMul(s.violation, b.violation)))
    override def abs = ???
    override def toInt = violation
    override def toDouble = violation.toDouble
    override def isEven = ???
    override def eqc(that: BooleanValue) =
        if (this.truthValue == that.truthValue) True
        else BooleanValue.get(safeInc(safeAdd(this.violation, that.violation)) / 2)
    override def nec(that: BooleanValue) =
        if (this.truthValue && that.truthValue) False
        else if (! this.truthValue && ! that.truthValue) BooleanValue.get(safeAdd(this.violation, that.violation) / 2)
        else True
    override def ltc(that: BooleanValue) =
        if (this.truthValue) BooleanValue.get(safeInc(that.violation)) else that
    override def lec(that: BooleanValue) =
        if (this.truthValue) BooleanValue.get(safeInc(that.violation) / 2) else True
}

/**
 * Companion object to BooleanValue.
 *
 * @author Michael Marte
 */
final object BooleanValue {

    implicit def valueTraits = BooleanValueTraits

    private val VALUE_RANGE = new Range(0, 10000, 1)
    private val valueCache = VALUE_RANGE.map(new BooleanValue(_)).toArray

    /**
     * Returns a BooleanValue instance for the given violation.
     *
     * Violations in VALUE_RANGE are used as index into an array of prefabricated
     * BooleanValue instances, so the operation is cheap for them.
     * For other violations, a new BooleanValue instance is created.
     */
    def get(a: Int): BooleanValue =
        if (VALUE_RANGE.contains(a)) valueCache.apply(a - VALUE_RANGE.start) else new BooleanValue(a)

}
