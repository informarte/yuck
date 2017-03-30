package yuck.core

import scala.math._

/**
 * Represents immutable integer ranges in terms of lower and upper bounds.
 *
 * A range is empty when its upper bound is smaller than its lower bound;
 * so the empty range has no normal form.
 *
 * Null bounds imply infinity.
 *
 * Currently only used as building block of [[yuck.core.IntegerDomain IntegerDomain]].
 *
 * @author Michael Marte
 */
final class IntegerRange(
    override val lb: IntegerValue, override val ub: IntegerValue)
    extends NumericalDomain[IntegerValue]
{
    override def hashCode = 3 * (3 + lb.hashCode) + ub.hashCode
    override def equals(that: Any) = that match {
        case rhs: IntegerRange => {
            val lhs = this
            lhs.lb == rhs.lb && lhs.ub == rhs.ub
        }
        case _ => false
    }
    override def toString =
        if (isEmpty)
            "{}"
        else if (isSingleton)
            "{%s}".format(singleValue)
        else
            "%s..%s".format(
                if (lb == null) "-inf" else lb.toString,
                if (ub == null) "+inf" else ub.toString)
    override def valueTraits = IntegerValueTraits
    override def size = {
        require(isFinite)
        0.max(ub.value - lb.value + 1)
    }
    override def isFinite = lb != null && ub != null
    override def values = {
        require(isFinite)
        (lb.value to ub.value).toIterator.map(a => IntegerValue.get(a))
    }
    override def contains(a: IntegerValue) =
        (lb == null || lb <= a) && (ub == null || a <= ub)
    override def singleValue = {
        require(isSingleton)
        lb
    }
    override def randomValue(randomGenerator: RandomGenerator) = {
        require(size > 0)
        IntegerValue.get(lb.value + randomGenerator.nextInt(size))
    }
    override def nextRandomValue(randomGenerator: RandomGenerator, currentValue: IntegerValue) = {
        require(size > 1)
        if (size == 2) {
            if (currentValue == lb) ub else lb
        } else {
            val a = lb.value + randomGenerator.nextInt(size - 1)
            IntegerValue.get(if (a < currentValue.value) a else a + 1)
        }
    }
    override def isBounded = lb != null || ub != null
    override def maybeLb = if (lb == null) None else Some(lb)
    override def maybeUb = if (ub == null) None else Some(ub)
    override def hull = this
    override def isSubsetOf(that: Domain[IntegerValue]) =
        valueTraits.isSubsetOf(this, that)
    def isSubsetOf(that: IntegerRange): Boolean = {
        val lhs = this
        var rhs = that
        ! lhs.startsBefore(rhs) && ! lhs.endsAfter(rhs)
    }
    def precedes(that: IntegerRange): Boolean = {
        val lhs = this
        val rhs = that
        lhs.ub != null && rhs.lb != null && lhs.ub < rhs.lb
    }
    def precedesImmediately(that: IntegerRange): Boolean = {
        val lhs = this
        val rhs = that
        lhs.ub != null && rhs.lb != null && lhs.ub + One == rhs.lb
    }
    def startsBefore(that: IntegerRange): Boolean = {
        val lhs = this
        val rhs = that
        if (lhs.lb == null) rhs.lb != null
        else if (rhs.lb != null) lhs.lb < rhs.lb
        else false
    }
    def startsAfter(that: IntegerRange): Boolean = {
        val lhs = this
        val rhs = that
        lhs.lb != null && (rhs.lb == null || lhs.lb > rhs.lb)
    }
    def endsBefore(that: IntegerRange): Boolean = {
        val lhs = this
        val rhs = that
        lhs.ub != null && (rhs.ub == null || lhs.ub < rhs.ub)
    }
    def endsAfter(that: IntegerRange): Boolean = {
        val lhs = this
        val rhs = that
        if (lhs.ub == null) rhs.ub != null
        else if (rhs.ub != null) lhs.ub > rhs.ub
        else false
    }
    def intersects(that: IntegerRange): Boolean =
        ! (this.precedes(that) || that.precedes(this))
    def intersect(that: IntegerRange): IntegerRange = {
        val lb =
            if (this.lb == null) that.lb
            else if (that.lb == null) this.lb
            else if (this.lb < that.lb) that.lb
            else this.lb
        val ub =
            if (this.ub == null) that.ub
            else if (that.ub == null) this.ub
            else if (this.ub < that.ub) this.ub
            else that.ub
        new IntegerRange(lb, ub)
    }
    def maybeIntersectionSize(that: IntegerRange): Option[Int] = {
        val tmp = intersect(that)
        if (tmp.isInfinite) None else Some(tmp.size)
    }
    override def boundFromBelow(lb: IntegerValue) = this.intersect(new IntegerRange(lb, null))
    override def boundFromAbove(ub: IntegerValue) = this.intersect(new IntegerRange(null, ub))
    override def bisect = {
        require(! isEmpty)
        require(isFinite)
        val mid = lb + ((ub - lb + One) / Two)
        (this.intersect(new IntegerRange(lb, mid - One)), this.intersect(new IntegerRange(mid, ub)))
    }
}
