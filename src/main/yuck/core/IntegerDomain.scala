package yuck.core

import scala.collection._

/**
 * Provides an interface for working with integer domains.
 *
 * @author Michael Marte
 */
abstract class IntegerDomain extends NumericalDomain[IntegerValue] {

    import IntegerDomain._

    final override def valueTraits = IntegerValueTraits

    final override def equals(that: Domain[IntegerValue]): Boolean = (this, that) match {
        case (lhs: IntegerRange, rhs: IntegerRange) => lhs.equals(rhs)
        case (lhs: IntegerRange, rhs: IntegerRangeList) =>
            (lhs.isEmpty && rhs.isEmpty) || (rhs.ranges.size == 1 && lhs.equals(rhs.ranges.head))
        case (lhs: IntegerRangeList, rhs: IntegerRangeList) => lhs.equals(rhs)
        case (lhs: IntegerRangeList, rhs: IntegerRange) =>
            (lhs.isEmpty && rhs.isEmpty) || (lhs.ranges.size == 1 && rhs.equals(lhs.ranges.head))
        case _ => ???
    }
    final override def hashCode =
        (3 * (3 + (if (isEmpty || lb == null) 0 else lb.hashCode)) +
            (if (isEmpty || ub == null) 0 else ub.hashCode))

    final override def compare(that: Domain[IntegerValue]): Int = (this, that) match {
        case (lhs: IntegerRange, rhs: IntegerRange) =>
            IntegerRange.ordering.compare(lhs, rhs)
        case (lhs: IntegerRange, rhs: IntegerRangeList) =>
            IntegerRangeList.ordering.compare(ensureRangeList(lhs), rhs)
        case (lhs: IntegerRangeList, rhs: IntegerRangeList) =>
            IntegerRangeList.ordering.compare(lhs, rhs)
        case (lhs: IntegerRangeList, rhs: IntegerRange) =>
            IntegerRangeList.ordering.compare(lhs, ensureRangeList(rhs))
        case _ => ???
    }

    override def hull: IntegerRange

    /** Return true iff the domain has at least one gap. */
    def hasGaps: Boolean

    final override def isSubsetOf(that: Domain[IntegerValue]): Boolean = (this, that) match {
        case (lhs: IntegerRange, rhs: IntegerRange) => lhs.isSubsetOf(rhs)
        case _ => ensureRangeList(this).isSubsetOf(ensureRangeList(that))
    }
    final override def intersects(that: Domain[IntegerValue]): Boolean = (this, that) match {
        case (lhs: IntegerRange, rhs: IntegerRange) => lhs.intersects(rhs)
        case _ => ensureRangeList(this).intersects(ensureRangeList(that))
    }
    final override def intersect(that: Domain[IntegerValue]): IntegerDomain = (this, that) match {
        case (lhs: IntegerRange, rhs: IntegerRange) => lhs.intersect(rhs)
        case _ => ensureRangeList(this).intersect(ensureRangeList(that))
    }
    final override def union(that: Domain[IntegerValue]): IntegerDomain =
        ensureRangeList(this).union(ensureRangeList(that))
    final override def diff(that: Domain[IntegerValue]): IntegerDomain =
        ensureRangeList(this).diff(ensureRangeList(that))

    /**
     * Computes the size of the intersection of this and that.
     *
     * Returns None when the intersection is infinite.
     */
    def maybeIntersectionSize(that: IntegerDomain): Option[Int] = (this, that) match {
        case (lhs: IntegerRange, rhs: IntegerRange) => lhs.maybeIntersectionSize(rhs)
        case _ => ensureRangeList(this).maybeIntersectionSize(ensureRangeList(that))
    }

    /**
     * Computes the size of this \ that.
     *
     * Returns None when the residue is infinite.
     */
    def maybeResidueSize(that: IntegerDomain): Option[Int] = {
        val lhs = this
        val rhs = that
        if (lhs.isFinite) Some(lhs.size - lhs.maybeIntersectionSize(rhs).get)
        else if (lhs.isSubsetOf(rhs)) Some(0)
        else {
            val residue = lhs.diff(rhs)
            if (residue.isFinite) Some(residue.size)
            else None
        }
    }

    /**
     * Returns 0 if the domain contains the given value;
     * otherwise returns the distance of the given value to the nearest range.
     *
     * Throws when the domain is empty.
     */
    def distanceTo(a: IntegerValue): Int

    /**
     * Chooses a random subdomain from the domain.
     *
     * Throws when the domain is infinite.
     */
    final def randomSubdomain(randomGenerator: RandomGenerator): IntegerDomain = {
        val choice = new mutable.HashSet[IntegerValue]
        valuesIterator.foreach(a => if (randomGenerator.nextDecision) choice += a)
        createDomain(choice)
    }

    /**
     * Chooses a random, non-empty subrange from the domain.
     *
     * Throws when the domain is infinite.
     */
    def randomSubrange(randomGenerator: RandomGenerator): IntegerRange

    /**
     * Returns true iff this precedes that.
     *
     * Throws when either range is empty.
     */
    final def precedes(that: IntegerDomain): Boolean = {
        require(! this.isEmpty)
        require(! that.isEmpty)
        this.hasUb && that.hasLb && this.ub < that.lb
    }

    /**
     * Returns true iff this precedes that and there is no gap between them.
     *
     * Throws when either range is empty.
     */
    final def precedesImmediately(that: IntegerDomain): Boolean = {
        require(! this.isEmpty)
        require(! that.isEmpty)
        this.hasUb && that.hasLb && this.ub + One == that.lb
    }

    /**
     * Returns true iff this starts before that.
     *
     * Throws when either range is empty.
     */
    final def startsBefore(that: IntegerDomain): Boolean = {
        require(! this.isEmpty)
        require(! that.isEmpty)
        if (! this.hasLb) that.hasLb
        else if (that.hasLb) this.lb < that.lb
        else false
    }

    /**
     * Returns true iff this starts after that.
     *
     * Throws when either range is empty.
     */
    final def startsAfter(that: IntegerDomain): Boolean = {
        require(! this.isEmpty)
        require(! that.isEmpty)
        this.hasLb && (! that.hasLb || this.lb > that.lb)
    }

    /**
     * Returns true iff this ends before that.
     *
     * Throws when either range is empty.
     */
    final def endsBefore(that: IntegerDomain): Boolean = {
        require(! this.isEmpty)
        require(! that.isEmpty)
        this.hasUb && (! that.hasUb || this.ub < that.ub)
    }

    /**
     * Returns true iff this ends after that.
     *
     * Throws when either range is empty.
     */
    final def endsAfter(that: IntegerDomain): Boolean = {
        require(! this.isEmpty)
        require(! that.isEmpty)
        if (! this.hasUb) that.hasUb
        else if (that.hasUb) this.ub > that.ub
        else false
    }

}

/**
 * Companion object to IntegerDomain.
 *
 * @author Michael Marte
 */
final object IntegerDomain {

    /** A total ordering on integer domains. */
    val ordering = new Ordering[IntegerDomain] {
        override def compare(lhs: IntegerDomain, rhs: IntegerDomain) = lhs.compare(rhs)
    }

    /** Turns the given integer domain into a range list, if necessary. */
    def ensureRangeList(domain: Domain[IntegerValue]): IntegerRangeList = domain match {
        case range: IntegerRange => if (range.isEmpty) EmptyIntegerRangeList else new IntegerRangeList(range)
        case rangeList: IntegerRangeList => rangeList
        case _ => ???
    }

    /**
     * Creates an integer domain from a sorted sequence of non-empty, disjoint,
     * and non-adjacent integer ranges.
     *
     * Tries to avoid memory allocation by re-using existing objects.
     */
    def createDomain(ranges: Seq[IntegerRange]): IntegerDomain =
        if (ranges.isEmpty) EmptyIntegerRange
        else if (ranges.size == 1) ranges.head
        else new IntegerRangeList(ranges.toIndexedSeq)

    /** Creates an integer domain from a given value set. */
    def createDomain(values: Iterable[IntegerValue]): IntegerDomain =
        if (values.isEmpty) {
            EmptyIntegerRange
        } else {
            val inIt =
                if (values.isInstanceOf[SortedSet[IntegerValue]]) values.iterator
                else values.toBuffer.sorted.iterator
            val outBuf = new mutable.ArrayBuffer[IntegerRange]
            var lb = inIt.next
            var ub = lb
            while (inIt.hasNext) {
                val a = inIt.next
                if (a.value <= safeInc(ub.value)) {
                    ub = a
                } else {
                    outBuf += new IntegerRange(lb, ub)
                    lb = a
                    ub = lb
                }
            }
            outBuf += new IntegerRange(lb, ub)
            createDomain(outBuf)
        }

    /**
     * Creates an integer range from the given boundaries.
     *
     * Tries to avoid memory allocation by re-using existing objects.
     */
    def createRange(lb: IntegerValue, ub: IntegerValue): IntegerRange =
        if (lb == null && ub == null) CompleteIntegerRange
        else if (lb != null && ub != null && ub < lb) EmptyIntegerRange
        else new IntegerRange(lb, ub)

}
