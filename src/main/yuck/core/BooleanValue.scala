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
final class BooleanValue(val violation: Long) extends NumericalValue[BooleanValue] {
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
    override def toInt = safeToInt(violation)
    override def toLong = violation
    override def toDouble = violation.toDouble
    override def isEven = ???
}

/**
 * Companion object to BooleanValue.
 *
 * @author Michael Marte
 */
final object BooleanValue {

    implicit def valueTraits = BooleanValueTraits

    private val valueRange = new Range(0, 10000, 1)
    private val valueCache = valueRange.map(new BooleanValue(_)).toArray

    /**
     * Returns a BooleanValue instance for the given violation.
     *
     * Violations in valueRange are used as index into an array of prefabricated
     * BooleanValue instances, so the operation is cheap for them.
     * For other violations, a new BooleanValue instance is created.
     */
    def get(a: Int): BooleanValue =
        if (valueRange.contains(a)) valueCache.apply(a - valueRange.start) else new BooleanValue(a)

    def get(a: Long): BooleanValue =
        if (a >= Int.MinValue && a <= Int.MaxValue) get(a.toInt) else new BooleanValue(a)

}
