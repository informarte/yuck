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
final class BooleanValue(val violation: Long) extends OrderedValue[BooleanValue] {
    require(violation >= 0)
    def this(value: Boolean) = this(if (value) 0 else 1)
    override def hashCode = violation.hashCode
    override def equals(that: Any) = that match {
        case rhs: BooleanValue =>
            val lhs = this
            lhs.violation == rhs.violation
        case _ => false
    }
    override def compare(that: BooleanValue) =
        if (this.violation < that.violation) -1
        else if (this.violation > that.violation) 1
        else 0
    inline override def ==(that: Value[BooleanValue]) = this == that.asInstanceOf[BooleanValue]
    inline def ==(that: BooleanValue): Boolean = this.violation == that.violation
    inline def !=(that: BooleanValue): Boolean = this.violation != that.violation
    inline override def <(that: BooleanValue) = this.violation < that.violation
    inline override def <=(that: BooleanValue) = this.violation <= that.violation
    inline override def >(that: BooleanValue) = this.violation > that.violation
    inline override def >=(that: BooleanValue) = this.violation >= that.violation
    override def toString =
        if (violation == 0) "true" else if (violation == 1) "false" else "false(%d)".format(violation)
    /** Returns true iff the violation is zero. */
    inline def truthValue: Boolean = violation == 0
}

/**
 * Companion object to BooleanValue.
 *
 * @author Michael Marte
 */
object BooleanValue {

    private val ub = 10000
    private val valueRange = Range(0, ub, 1)
    private val valueCache = valueRange.map(new BooleanValue(_)).toArray

    /**
     * Returns a BooleanValue instance for the given truth value.
     *
     * Tries to avoid memory allocation by re-using existing objects.
     */
    inline def apply(a: Boolean): BooleanValue =
        if (a) True else False

    /**
     * Returns a BooleanValue instance for the given violation.
     *
     * Violations in valueRange are used as index into an array of prefabricated
     * BooleanValue instances, so the operation is cheap for them.
     * For other violations, a new BooleanValue instance is created.
     */
    def apply(a: Int): BooleanValue =
        if (a < ub) valueCache(a) else new BooleanValue(a)

    /**
     * Returns a BooleanValue instance for the given violation.
     *
     * Violations in valueRange are used as index into an array of prefabricated
     * BooleanValue instances, so the operation is cheap for them.
     * For other violations, a new BooleanValue instance is created.
     */
    def apply(a: Long): BooleanValue =
        if (a >= Int.MinValue && a <= Int.MaxValue) BooleanValue(a.toInt) else new BooleanValue(a)

}
