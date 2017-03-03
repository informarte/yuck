package yuck.core

import scala.collection._
import scala.annotation.tailrec

/**
 * Implements immutable integer domains with holes as lists of non-empty, disjoint,
 * and non-adjacent integer ranges.
 *
 * The empty domain is represented as the empty list.
 *
 * Takes shortcuts in single-range cases.
 *
 * @author Michael Marte
 */
final class IntegerDomain(
    val ranges: immutable.IndexedSeq[IntegerRange])
    extends NumericalDomain[IntegerValue]
{

    for (range <- ranges) {
        require(! range.isEmpty)
    }
    for (i <- 1 until ranges.size) {
        require(ranges(i - 1).ub + One < ranges(i).lb)
    }

    def this() = this(immutable.IndexedSeq[IntegerRange]())
    def this(a: IntegerValue) = this(immutable.IndexedSeq(new IntegerRange(a, a)))
    def this(a: IntegerValue, b: IntegerValue) = this(
        if (a != null && b != null && b < a) immutable.IndexedSeq()
        else immutable.IndexedSeq(new IntegerRange(a, b)))
    def this(values: Set[IntegerValue]) = this(IntegerDomain.buildRanges(values).toIndexedSeq)

    override def isBounded = isEmpty || (lb != null || ub != null)
    @inline override def lb = if (isEmpty) One else ranges.head.lb
    @inline override def ub = if (isEmpty) Zero else ranges.last.ub
    override def hull: IntegerRange = if (ranges.size == 1) ranges.head else new IntegerRange(lb, ub)

    override def hashCode =
        3 * (3 + (if (isEmpty || lb == null) 0 else lb.hashCode)) +
        (if (isEmpty || ub == null) 0 else ub.hashCode)
    override def equals(that: Any) = that match {
        case rhs: IntegerDomain => {
            val lhs = this
            lhs.eq(rhs) || lhs.ranges == rhs.ranges
        }
        case _ => false
    }
    override def toString = if (isEmpty) "{}" else ranges.map(_.toString).mkString(" union ")

    override def valueTraits = IntegerValueTraits
    @inline override def isEmpty = ranges.isEmpty
    override lazy val size = ranges.map(_.size).sum
    override def isFinite = isEmpty || (lb != null && ub != null)

    lazy val sparse = isFinite && ranges.size.toFloat / size.toFloat > 0.5
    /** Decides whether the domain is sparse. */
    @inline def isSparse: Boolean = sparse
    /** Decides whether the domain is not sparse. */
    @inline def isDense: Boolean = ! isSparse

    private lazy val rangeDistribution = IntegerDomain.createRangeDistribution(ranges)

    override def contains(a: IntegerValue) = findIndexOfContainingRange(a, 0, ranges.size - 1) >= 0
    override def values = {
        require(isFinite)
        ranges.toIterator.flatMap(_.values)
    }
    override def singleValue = {
        require(isSingleton)
        lb
    }

    override def randomValue(randomGenerator: RandomGenerator) = {
        require(! isEmpty)
        if (ranges.size == 1) ranges.head.randomValue(randomGenerator)
        else ranges(rangeDistribution.nextIndex(randomGenerator)).randomValue(randomGenerator)
    }

    override def nextRandomValue(randomGenerator: RandomGenerator, currentValue: IntegerValue) = {
        require(! isEmpty)
        if (isSingleton) {
            singleValue
        } else if (size == 2) {
            if (currentValue == lb) ub else lb
        } else if (ranges.size == 1) {
            ranges.head.nextRandomValue(randomGenerator, currentValue)
        } else {
            val i = findIndexOfContainingRange(currentValue, 0, ranges.size - 1)
            try {
                rangeDistribution.addFrequencyDelta(i, -1)
                val j = rangeDistribution.nextIndex(randomGenerator)
                val range = ranges(j)
                if (range.contains(currentValue)) {
                    range.nextRandomValue(randomGenerator, currentValue)
                } else {
                    range.randomValue(randomGenerator)
                }
            } finally {
                rangeDistribution.addFrequencyDelta(i, +1)
            }
        }
    }

    override def isSubsetOf(that: Domain[IntegerValue]) =
        valueTraits.isSubsetOf(this, that)

    /** Decides whether this is a subset of that. */
    def isSubsetOf(that: IntegerDomain): Boolean = {
        val lhs = this
        val rhs = that
        if (lhs.isEmpty) true
        else if (lhs.lb == null) IntegerDomain.isSubsetOf(lhs, 0, rhs, 0)
        else {
            val i = rhs.findIndexOfContainingRange(lhs.lb, 0, rhs.ranges.size - 1)
            i >= 0 && IntegerDomain.isSubsetOf(lhs, 0, rhs, i)
        }
    }

    /** Computes the intersection of this and that. */
    def intersect(that: IntegerDomain): IntegerDomain = {
        if (this.cannotIntersect(that)) EmptyIntegerDomain
        else {
            val i = this.findIndexOfFirstIntersectingRange(that.hull, 0, this.ranges.size - 1)
            if (i < 0) EmptyIntegerDomain
            else {
                val buf = new mutable.ArrayBuffer[IntegerRange](scala.math.max(this.ranges.size, that.ranges.size))
                IntegerDomain.intersect(this, i, that, 0, buf)
                new IntegerDomain(buf.toIndexedSeq)
            }
        }
    }

    /**
     * Computes the size of the intersection of this and that.
     *
     * Returns None when the intersection is infinite.
     */
    def maybeIntersectionSize(that: IntegerDomain): Option[Int] = {
        if (this.cannotIntersect(that)) Some(0)
        else {
            val i = this.findIndexOfFirstIntersectingRange(that.hull, 0, this.ranges.size - 1)
            if (i < 0) Some(0)
            else IntegerDomain.maybeIntersectionSize(this, i, that, 0, 0)
        }
    }

    /** Decides whether this intersects that. */
    def intersects(that: IntegerDomain): Boolean = {
        if (this.cannotIntersect(that)) false
        else {
            val i = this.findIndexOfFirstIntersectingRange(that.hull, 0, this.ranges.size - 1)
            if (i < 0) false
            else IntegerDomain.intersects(this, i, that, 0)
        }
    }

    /**
     * Returns 0 if the domain contains the given value;
     * otherwise returns the distance of the given value to the nearest range.
     */
    def distance(a: IntegerValue): Int =
        if (isEmpty) 1
        else if (lb != null && a < lb) lb.value - a.value
        else if (ub != null && a > ub) a.value - ub.value
        else if (ranges.size == 1) 0
        else {
            val i = findIndexOfContainingHole(a, 0, ranges.size - 2)
            if (i < 0) 0
            else scala.math.min(a.value - ranges(i).ub.value, ranges(i + 1).lb.value - a.value)
        }

    /** Computes this \ that. */
    def subtract(that: IntegerDomain): IntegerDomain = {
        val lhs = this
        val rhs = that
        if (lhs.cannotIntersect(rhs)) lhs
        else {
            val buf = new mutable.ArrayBuffer[IntegerRange](lhs.ranges.size * 2)
            IntegerDomain.subtract(lhs, 0, rhs, 0, buf)
            new IntegerDomain(buf.toIndexedSeq)
        }
    }

    /**
     * Computes the size of this \ that.
     *
     * Returns None when the residue is infinite.
     */
    def maybeResidueSize(that: IntegerDomain): Option[Int] = {
        val lhs = this
        val rhs = that
        if (rhs.isUnbounded) Some(0)
        else if (lhs.isFinite) Some(lhs.size - lhs.maybeIntersectionSize(rhs).get)
        else if (lhs.isSubsetOf(rhs)) Some(0)
        else None
    }

    /** Computes the union of this and that. */
    def unite(that: IntegerDomain): IntegerDomain = {
        val buf = new mutable.ArrayBuffer[IntegerRange](this.ranges.size + that.ranges.size)
        IntegerDomain.unite(this, 0, that, 0, buf)
        new IntegerDomain(buf.toIndexedSeq)
    }

    override def boundFromBelow(lb: IntegerValue) = this.intersect(new IntegerDomain(lb, null))

    override def boundFromAbove(ub: IntegerValue) = this.intersect(new IntegerDomain(null, ub))

    override def bisect = {
        require(! isEmpty)
        require(isFinite)
        val mid = lb + ((ub - lb + One) / Two)
        (this.intersect(new IntegerDomain(lb, mid - One)), this.intersect(new IntegerDomain(mid, ub)))
    }

    private def cannotIntersect(that: IntegerDomain): Boolean = {
        this.isEmpty ||
        that.isEmpty ||
        that.ranges.last.precedes(this.ranges.head) ||
        this.ranges.last.precedes(that.ranges.head)
    }

    @tailrec
    private def findIndexOfFirstIntersectingRange(r: IntegerRange, start: Int, end: Int): Int =
        if (start > end) -1
        else {
            val mid = start + (end - start + 1) / 2
            if (ranges(mid).intersects(r)) {
                if (mid > 0 && ranges(mid - 1).intersects(r)) {
                    findIndexOfFirstIntersectingRange(r, start, mid - 1)
                } else {
                    mid
                }
            }
            else if (r.precedes(ranges(mid))) findIndexOfFirstIntersectingRange(r, start, mid - 1)
            else findIndexOfFirstIntersectingRange(r, mid + 1, end)
        }

    @tailrec
    private def findIndexOfContainingRange(a: IntegerValue, start: Int, end: Int): Int =
        if (start > end) -1
        else {
            val mid = start + (end - start + 1) / 2
            if (ranges(mid).contains(a)) mid
            else if (mid > 0 && ranges(mid).lb > a) findIndexOfContainingRange(a, start, mid - 1)
            else findIndexOfContainingRange(a, mid + 1, end)
        }

    @tailrec
    private def findIndexOfContainingHole(a: IntegerValue, start: Int, end: Int): Int =
        if (start > end) -1
        else {
            val mid = start + (end - start + 1) / 2
            if (ranges(mid).ub < a && a < ranges(mid + 1).lb) mid
            else if (ranges(mid).lb > a) findIndexOfContainingHole(a, start, mid - 1)
            else findIndexOfContainingHole(a, mid + 1, end)
        }

}

/**
 * Provides tools for the implementation of integer domains.
 *
 * @author Michael Marte
 */
final object IntegerDomain {

    private def createRangeDistribution(ranges: immutable.IndexedSeq[IntegerRange]): Distribution = {
        val result = DistributionFactory.createDistribution(ranges.size)
        for (i <- ranges.indices) {
            result.setFrequency(i, ranges(i).size)
        }
        result
    }

    private def collateAdjacentRanges(ranges: List[IntegerRange]): List[IntegerRange] = ranges match {
        case (r1 :: r2 :: t) if r1.ub + One == r2.lb => collateAdjacentRanges(new IntegerRange(r1.lb, r2.ub) :: t)
        case r :: t => r :: collateAdjacentRanges(t)
        case _ => Nil
    }

    private def buildRanges(values: Set[IntegerValue]): List[IntegerRange] =
        collateAdjacentRanges(values.toList.sorted.map(a => new IntegerRange(a, a)))

    // In the following methods we prefer indices and tail recursion over iterators and loops
    // to avoid the overhead of creating ranges and iterators.
    // Generalizing the methods is actually not a one-liner because we need a C++ like iterator
    // that can be dereferenced and advanced independently and such an iterator is not available
    // in Scala.
    // (BufferedIterator looks like a candidate but, while it has a head method, it lacks advance.
    //  drop could be used instead but it returns a plain iterator implying the need to create
    //  a new BufferedIterator object for use in recursion.)
    // While providing a suitable wrapper for Iterator would not be too difficult, there is no way
    // to avoid the performance implications of using it:  After creating the plain iterator from
    // the collection, it has to be wrapped, such that in total four objects must be created for one
    // application of isSubsetOf, for example.

    @tailrec
    private def isSubsetOf(lhs: IntegerDomain, i: Int, rhs: IntegerDomain, j: Int): Boolean =
        if (i == lhs.ranges.size) true
        else if (j == rhs.ranges.size) false
        else if (lhs.ranges(i).precedes(rhs.ranges(j))) false
        else if (lhs.ranges(i).isSubsetOf(rhs.ranges(j))) isSubsetOf(lhs, i + 1, rhs, j)
        else isSubsetOf(lhs, i, rhs, j + 1)

    @tailrec
    private def intersect(lhs: IntegerDomain, i: Int, rhs: IntegerDomain, j: Int, buf: mutable.Buffer[IntegerRange]) {
        if (i == lhs.ranges.size) {}
        else if (j == rhs.ranges.size) {}
        else {
            val r = lhs.ranges(i)
            val u = rhs.ranges(j)
            if (r.precedes(u)) intersect(lhs, i + 1, rhs, j, buf)
            else if (u.precedes(r)) intersect(lhs, i, rhs, j + 1, buf)
            else {
               buf += r.intersect(u)
               intersect(
                   lhs, if (u.endsBefore(r)) i else i + 1,
                   rhs, if (r.endsBefore(u)) j else j + 1,
                   buf)
            }
        }
    }

    @tailrec
    private def maybeIntersectionSize(lhs: IntegerDomain, i: Int, rhs: IntegerDomain, j: Int, n: Int): Option[Int] =
        if (i == lhs.ranges.size) Some(n)
        else if (j == rhs.ranges.size) Some(n)
        else {
            val r = lhs.ranges(i)
            val u = rhs.ranges(j)
            if (r.precedes(u)) maybeIntersectionSize(lhs, i + 1, rhs, j, n)
            else if (u.precedes(r)) maybeIntersectionSize(lhs, i, rhs, j + 1, n)
            else {
                val maybeM = r.maybeIntersectionSize(u)
                if (maybeM.isEmpty) None
                else maybeIntersectionSize(
                         lhs, if (u.endsBefore(r)) i else i + 1,
                         rhs, if (r.endsBefore(u)) j else j + 1,
                         n + maybeM.get)
            }
        }

    @tailrec
    private def intersects(lhs: IntegerDomain, i: Int, rhs: IntegerDomain, j: Int): Boolean =
        if (i == lhs.ranges.size) false
        else if (j == rhs.ranges.size) false
        else {
            val r = lhs.ranges(i)
            val u = rhs.ranges(j)
            if (r.precedes(u)) intersects(lhs, i + 1, rhs, j)
            else if (u.precedes(r)) intersects(lhs, i, rhs, j + 1)
            else
               r.intersects(u) ||
               intersects(
                   lhs, if (u.endsBefore(r)) i else i + 1,
                   rhs, if (r.endsBefore(u)) j else j + 1)
        }

    @tailrec
    private def subtract(lhs: IntegerDomain, i: Int, rhs: IntegerDomain, j: Int, buf: mutable.Buffer[IntegerRange]) {
        if (i == lhs.ranges.size) {
        }
        else {
            val k = subtract(lhs.ranges(i), rhs, j, buf)
            subtract(lhs, i + 1, rhs, k, buf)
        }
    }

    @tailrec
    private def subtract(lhs: IntegerRange, rhs: IntegerDomain, j: Int, buf: mutable.Buffer[IntegerRange]): Int = {
        if (j == rhs.ranges.size) {
            buf += lhs
            j
        } else {
            // r \ u
            val r = lhs
            val u = rhs.ranges(j)
            if (u.precedes(r)) {
                subtract(r, rhs, j + 1, buf)
            }
            else if (r.precedes(u)) {
                buf += r
                j
            }
            else if (u.endsBefore(r)) {
                if (u.startsAfter(r)) {
                    buf += new IntegerRange(r.lb, u.lb - One)
                }
                subtract(new IntegerRange(u.ub + One, r.ub), rhs, j + 1, buf)
            }
            else if (u.startsAfter(r)) {
                buf += new IntegerRange(r.lb, u.lb - One)
                j
            }
            else {
                assert(r.isSubsetOf(u))
                j
            }
        }
    }

    @tailrec
    private def unite(lhs: IntegerDomain, i: Int, rhs: IntegerDomain, j: Int, buf: mutable.Buffer[IntegerRange]) {
        def addRange(u: IntegerRange) {
            if (buf.isEmpty) {
                buf += u
            } else {
                val r = buf.last
                assert(! r.startsAfter(u))
                if (r.precedesImmediately(u) || r.intersects(u)) {
                    buf.update(buf.size - 1, new IntegerRange(r.lb, if (r.endsAfter(u)) r.ub else u.ub))
                } else {
                    buf += u
                }
            }
        }
        if (i == lhs.ranges.size) {
            if (j < rhs.ranges.size) {
                addRange(rhs.ranges(j))
                unite(lhs, i, rhs, j + 1, buf)
            }
        }
        else if (j == rhs.ranges.size) {
            addRange(lhs.ranges(i))
            unite(lhs, i + 1, rhs, j, buf)
        } else {
            val r = lhs.ranges(i)
            val u = rhs.ranges(j)
            if (u.startsAfter(r)) {
                addRange(r)
                unite(lhs, i + 1, rhs, j, buf)
            } else {
                addRange(u)
                unite(lhs, i, rhs, j + 1, buf)
            }
        }
    }

}
