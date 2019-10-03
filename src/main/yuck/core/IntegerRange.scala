package yuck.core

import IntegerDomain.createRange

/**
 * Represents immutable integer ranges in terms of lower and upper bounds.
 *
 * A range is empty when its upper bound is smaller than its lower bound;
 * so the empty range has no normal form.
 *
 * Null bounds imply infinity.
 *
 * @author Michael Marte
 */
final class IntegerRange
    (override val lb: IntegerValue, override val ub: IntegerValue)
    extends IntegerDomain
{

    def equals(that: IntegerRange): Boolean =
        this.eq(that) || (this.isEmpty && that.isEmpty) || this.lb == that.lb && this.ub == that.ub

    override def toString =
        if (isEmpty)
            "{}"
        else if (isSingleton)
            "{%s}".format(singleValue)
        else
            "%s..%s".format(
                if (lb == null) "-inf" else lb.toString,
                if (ub == null) "+inf" else ub.toString)

    override def size = {
        require(isFinite)
        max(0, safeInc(safeSub(ub.value, lb.value)))
    }
    override def isComplete = lb == null && ub == null
    override def isFinite = lb != null && ub != null
    override def isEmpty = isFinite && lb > ub
    override def isSingleton = isFinite && lb == ub
    override def hasGaps = false
    override def isBounded = lb != null || ub != null
    override def maybeLb = if (lb == null) None else Some(lb)
    override def maybeUb = if (ub == null) None else Some(ub)
    override def hull = this
    override def values = {
        require(isFinite)
        (lb.value to ub.value).view.map(a => IntegerValue.get(a))
    }
    override def valuesIterator = {
        require(isFinite)
        (lb.value to ub.value).iterator.map(a => IntegerValue.get(a))
    }
    override def singleValue = {
        require(isSingleton)
        lb
    }
    override def contains(a: IntegerValue) =
        (lb == null || lb <= a) && (ub == null || a <= ub)

    override def randomValue(randomGenerator: RandomGenerator) = {
        require(! isEmpty && isFinite)
        IntegerValue.get(lb.value + randomGenerator.nextInt(size))
    }

    override def nextRandomValue(randomGenerator: RandomGenerator, currentValue: IntegerValue) = {
        require(! isEmpty && isFinite)
        if (isSingleton) {
            singleValue
        } else if (size == 2) {
            if (currentValue == lb) ub else lb
        } else {
            val a = lb.value + randomGenerator.nextInt(size - 1)
            IntegerValue.get(if (a < currentValue.value) a else a + 1)
        }
    }

    override def boundFromBelow(lb: IntegerValue) = this.intersect(createRange(lb, null))

    override def boundFromAbove(ub: IntegerValue) = this.intersect(createRange(null, ub))

    override def bisect = {
        require(! isEmpty)
        require(isFinite)
        val mid = lb.value + (safeInc(ub.value - lb.value) / 2)
        (this.intersect(createRange(lb, IntegerValue.get(safeDec(mid)))),
         this.intersect(createRange(IntegerValue.get(mid), ub)))
    }

    override def distanceTo(a: IntegerValue) = {
        require(! isEmpty)
        if (lb != null && a < lb) safeSub(lb.value, a.value)
        else if (ub != null && a > ub) safeSub(a.value, ub.value)
        else 0
    }

    def isSubsetOf(that: IntegerRange): Boolean =
        this.isEmpty || (! that.isEmpty && ! this.startsBefore(that) && ! this.endsAfter(that))

    def intersects(that: IntegerRange): Boolean =
        ! this.isEmpty && ! that.isEmpty && ! (this.precedes(that) || that.precedes(this))

    def intersect(that: IntegerRange): IntegerRange = {
        val lb =
            if (! this.hasLb) that.lb
            else if (! that.hasLb) this.lb
            else if (this.lb < that.lb) that.lb
            else this.lb
        val ub =
            if (! this.hasUb) that.ub
            else if (! that.hasUb) this.ub
            else if (this.ub < that.ub) this.ub
            else that.ub
        IntegerDomain.createRange(lb, ub)
    }

    def maybeIntersectionSize(that: IntegerRange): Option[Int] = {
        val tmp = intersect(that)
        if (tmp.isFinite) Some(tmp.size) else None
    }

    override def randomSubrange(randomGenerator: RandomGenerator): IntegerRange =
        if (isEmpty) EmptyIntegerRange
        else {
            val a = randomValue(randomGenerator)
            val b = randomValue(randomGenerator)
            if (a < b) createRange(a, b) else createRange(b, a)
        }

    /**
     * Implements range multiplication as described in:
     * K. R. Apt, Principles of Constraint Programming, p. 221
     */
    def mult(that: IntegerRange): IntegerRange = {
        if (this.isEmpty || that.isEmpty) {
            EmptyIntegerRange
        } else {
            require(this.isFinite)
            require(that.isFinite)
            val a = this.lb.value
            val b = this.ub.value
            val c = that.lb.value
            val d = that.ub.value
            val A = List(safeMul(a, c), safeMul(a, d), safeMul(b, c), safeMul(b, d))
            createRange(IntegerValue.get(A.min), IntegerValue.get(A.max))
        }
    }

    /**
     * Implements range division as described in:
     * K. R. Apt, Principles of Constraint Programming, p. 221
     */
    def div(that: IntegerRange): IntegerRange = {
        if (this.isEmpty || that.isEmpty) {
            EmptyIntegerRange
        } else {
            require(this.isFinite)
            require(that.isFinite)
            val a = this.lb
            val b = this.ub
            val c = that.lb
            val d = that.ub
            if (this.contains(Zero) && that.contains(Zero)) {
                // case 1
                CompleteIntegerRange
            } else if (! this.contains(Zero) && c == Zero && d == Zero) {
                // case 2
                EmptyIntegerRange
            } else if (! this.contains(Zero) && c < Zero && Zero < d) {
                // case 3
                val e = IntegerValueTraits.valueOrdering.max(a.abs, b.abs)
                createRange(MinusOne * e, e)
            } else if (! this.contains(Zero) && c < Zero && d == Zero) {
                // case 4a
                this.div(createRange(c, MinusOne))
            } else if (! this.contains(Zero) && c == Zero && Zero < d) {
                // case 4b
                this.div(createRange(One, d))
            } else if (! that.contains(Zero)) {
                // case 5
                // approximation (6.14)
                val A = List(a.toDouble / c.toDouble, a.toDouble / d.toDouble, b.toDouble / c.toDouble, b.toDouble / d.toDouble)
                import scala.math.Ordering.Double.TotalOrdering
                createRange(IntegerValue.get(A.min.ceil.toInt), IntegerValue.get(A.max.floor.toInt))
            } else {
                // Must not occur since the preceding case distinction covers all cases.
                ???
            }
        }
    }

}

/**
 * Companion object to IntegerRange.
 *
 * @author Michael Marte
 */
object IntegerRange {
}
