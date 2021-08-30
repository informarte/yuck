package yuck.core

import scala.collection._

/**
 * Provides an interface for working with integer domains.
 *
 * @author Michael Marte
 */
abstract class IntegerDomain extends NumericalDomain[IntegerValue] {

    import IntegerDomain._

    final override def valueType = classOf[IntegerValue]

    final override def hashCode =
        (3 * (3 + (if (isEmpty || lb.eq(null)) 0 else lb.hashCode)) +
            (if (isEmpty || ub.eq(null)) 0 else ub.hashCode))

    final override def compare(that: OrderedDomain[IntegerValue]) = (this, that) match {
        case (lhs: IntegerRange, rhs: IntegerRange) =>
            rangeOrdering.compare(lhs, rhs)
        case (lhs: IntegerRange, rhs: IntegerRangeList) =>
            rangeListOrdering.compare(ensureRangeList(lhs), rhs)
        case (lhs: IntegerRangeList, rhs: IntegerRangeList) =>
            rangeListOrdering.compare(lhs, rhs)
        case (lhs: IntegerRangeList, rhs: IntegerRange) =>
            rangeListOrdering.compare(lhs, ensureRangeList(rhs))
        case _ => ???
    }
    final override def ==(that: Domain[IntegerValue]) = (this, that) match {
        case (lhs: IntegerRange, rhs: IntegerRange) => lhs == rhs
        case (lhs: IntegerRange, rhs: IntegerRangeList) =>
            (lhs.isEmpty && rhs.isEmpty) || (rhs.ranges.size == 1 && lhs == rhs.ranges.head)
        case (lhs: IntegerRangeList, rhs: IntegerRangeList) => lhs == rhs
        case (lhs: IntegerRangeList, rhs: IntegerRange) =>
            (lhs.isEmpty && rhs.isEmpty) || (lhs.ranges.size == 1 && rhs ==lhs.ranges.head)
        case _ => ???
    }
    inline final override def !=(that: Domain[IntegerValue]) = ! (this == that)

    override def hull: IntegerRange
    override def mirrored: IntegerDomain
    override def distanceTo(a: NumericalValue[IntegerValue]): IntegerValue

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

    final override def randomSubdomain(randomGenerator: RandomGenerator): IntegerDomain = {
        val choice = new mutable.HashSet[IntegerValue]
        valuesIterator.foreach(a => if (randomGenerator.nextDecision()) choice += a)
        apply(choice)
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
object IntegerDomain {

    private def rangeLessThan(lhs: IntegerRange, rhs: IntegerRange) =
        ! rhs.isSubsetOf(lhs) && (lhs.isSubsetOf(rhs) || lhs.startsBefore(rhs))

    // An intuitive ordering on integer ranges with A.isSubsetOf(B) -> A < B.
    // If neither A.isSubsetOf(B) nor B.isSubsetOf(A), then A < B <-> (lb(A), ub(A)) < (lb(B), ub(B)).
    // Yes, this is total ordering, I proved it on paper.
    private def rangeOrdering = Ordering.fromLessThan(rangeLessThan)

    // The lexicographic ordering on the underlying range lists.
    private val rangeListOrdering = new Ordering[IntegerRangeList] {
        val ordering = createLexicographicOrderingForIterable(rangeOrdering)
        override def compare(lhs: IntegerRangeList, rhs: IntegerRangeList) =
            ordering.compare(lhs.ranges, rhs.ranges)
    }

    /** Turns the given integer domain into a range list, if necessary. */
    def ensureRangeList(domain: Domain[IntegerValue]): IntegerRangeList = domain match {
        case range: IntegerRange => if (range.isEmpty) EmptyIntegerRangeList else IntegerRangeList(range)
        case rangeList: IntegerRangeList => rangeList
        case _ => ???
    }

    /**
     * Creates an integer domain from a sorted sequence of non-empty, disjoint,
     * and non-adjacent integer ranges.
     *
     * Tries to avoid memory allocation by re-using existing objects.
     */
    def apply(ranges: Seq[IntegerRange]): IntegerDomain =
        if (ranges.isEmpty) EmptyIntegerRange
        else if (ranges.size == 1) ranges.head
        else new IntegerRangeList(ranges.toIndexedSeq)

    /** Creates an integer domain from a given value set. */
    def apply(values: Iterable[IntegerValue]): IntegerDomain =
        if (values.isEmpty) {
            EmptyIntegerRange
        } else {
            val inIt =
                if (values.isInstanceOf[SortedSet[IntegerValue]]) values.iterator
                else values.toBuffer.sorted.iterator
            val outBuf = new mutable.ArrayBuffer[IntegerRange]
            var lb = inIt.next()
            var ub = lb
            while (inIt.hasNext) {
                val a = inIt.next()
                if (a.value <= safeInc(ub.value)) {
                    ub = a
                } else {
                    outBuf += IntegerRange(lb, ub)
                    lb = a
                    ub = lb
                }
            }
            outBuf += IntegerRange(lb, ub)
            IntegerDomain(outBuf)
        }

}
