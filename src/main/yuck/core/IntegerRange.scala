package yuck.core

import scala.annotation.tailrec

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

    def ==(that: IntegerRange): Boolean =
        this.eq(that) ||
        (this.isEmpty && that.isEmpty) ||
        ((this.lb.eq(that.lb) || (this.lb.ne(null) && that.lb.ne(null) && this.lb == that.lb)) &&
         (this.ub.eq(that.ub) || (this.ub.ne(null) && that.ub.ne(null) && this.ub == that.ub)))
    @inline def !=(that: IntegerRange): Boolean = ! (this == that)

    override def toString =
        if (isEmpty)
            "{}"
        else if (isSingleton)
            "{%s}".format(singleValue)
        else
            "%s..%s".format(
                if (lb.eq(null)) "-inf" else lb.toString,
                if (ub.eq(null)) "+inf" else ub.toString)

    override def size = {
        require(isFinite)
        max(0, safeInc(safeSub(ub.value, lb.value)))
    }
    @inline override def isComplete = lb.eq(null) && ub.eq(null)
    @inline override def isFinite = lb.ne(null) && ub.ne(null)
    @inline override def isEmpty = isFinite && lb > ub
    @inline override def isSingleton = isFinite && lb == ub
    override def hasGaps = false
    @inline override def isBounded = lb.ne(null) || ub.ne(null)
    @inline override def maybeLb = Option(lb)
    @inline override def maybeUb = Option(ub)
    override def hull = this
    override def values = {
        require(isFinite)
        (lb.value to ub.value).view.map(a => IntegerValue(a))
    }
    override def valuesIterator = {
        require(isFinite)
        // We don't use the Scala Range type to avoid integer boxing.
        new Iterator[IntegerValue] {
            private var i = lb.value
            override def hasNext = i <= ub.value
            override def next() = {
                val a = IntegerValue(i)
                i += 1
                a
            }
        }
    }
    override def singleValue = {
        require(isSingleton)
        lb
    }
    override def contains(a: IntegerValue) =
        (lb.eq(null) || lb <= a) && (ub.eq(null) || a <= ub)

    override def randomValue(randomGenerator: RandomGenerator) = {
        require(! isEmpty && isFinite)
        IntegerValue(lb.value + randomGenerator.nextInt(size))
    }

    override def nextRandomValue(randomGenerator: RandomGenerator, currentValue: IntegerValue) = {
        require(! isEmpty && isFinite)
        if (isSingleton) {
            singleValue
        } else if (size == 2) {
            if (currentValue == lb) ub else lb
        } else {
            val a = lb.value + randomGenerator.nextInt(size - 1)
            IntegerValue(if (a < currentValue.value) a else a + 1)
        }
    }

    override def boundFromBelow(lb: IntegerValue) = this.intersect(IntegerRange(lb, null))

    override def boundFromAbove(ub: IntegerValue) = this.intersect(IntegerRange(null, ub))

    override def bisect = {
        require(! isEmpty)
        require(isFinite)
        val mid = lb.value + (safeInc(ub.value - lb.value) / 2)
        (this.intersect(IntegerRange(lb, IntegerValue(safeDec(mid)))),
         this.intersect(IntegerRange(IntegerValue(mid), ub)))
    }

    override def distanceTo(a0: NumericalValue[IntegerValue]): IntegerValue = {
        require(! isEmpty)
        val a = a0.asInstanceOf[IntegerValue]
        if (lb.ne(null) && a < lb) lb - a
        else if (ub.ne(null) && a > ub) a - ub
        else Zero
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
        IntegerRange(lb, ub)
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
            if (a < b) IntegerRange(a, b) else IntegerRange(b, a)
        }

    override def mirrored: IntegerRange =
        if (isEmpty) this
        else IntegerRange(maybeUb.map(_.negate).orNull, maybeLb.map(_.negate).orNull)

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
            IntegerRange(A.min, A.max)
        }
    }

    /**
     * Implements range division as described in:
     * K. R. Apt, Principles of Constraint Programming, p. 221
     */
    @tailrec
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
                IntegerRange(MinusOne * e, e)
            } else if (! this.contains(Zero) && c < Zero && d == Zero) {
                // case 4a
                this.div(IntegerRange(c, MinusOne))
            } else if (! this.contains(Zero) && c == Zero && Zero < d) {
                // case 4b
                this.div(IntegerRange(One, d))
            } else if (! that.contains(Zero)) {
                // case 5
                // approximation (6.14)
                val A = List(a.toDouble / c.toDouble, a.toDouble / d.toDouble, b.toDouble / c.toDouble, b.toDouble / d.toDouble)
                import scala.math.Ordering.Double.TotalOrdering
                IntegerRange(A.min.ceil.toInt, A.max.floor.toInt)
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

    /**
     * Creates an IntegerRange instance from the given boundaries.
     *
     * Tries to avoid memory allocation by re-using existing objects.
     */
    def apply(lb: IntegerValue, ub: IntegerValue): IntegerRange =
        if (lb.eq(null) && ub.eq(null)) CompleteIntegerRange
        else if (lb.ne(null) && ub.ne(null) && ub < lb) EmptyIntegerRange
        else new IntegerRange(lb, ub)


    /**
     * Creates an integer range from the given boundaries.
     *
     * Tries to avoid memory allocation by re-using existing objects.
     */
    def apply(lb: Int, ub: Int): IntegerRange =
        if (ub < lb) EmptyIntegerRange
        else new IntegerRange(IntegerValue(lb), IntegerValue(ub))

}
