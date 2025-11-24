package yuck.core

import scala.annotation.tailrec
import scala.collection.*

/**
 * Implements immutable integer domains with holes as lists of non-empty, disjoint,
 * and non-adjacent integer ranges.
 *
 * The empty domain is represented as the empty list.
 *
 * Takes shortcuts in single-range cases.
 */
final class IntegerRangeList
    (val ranges: immutable.IndexedSeq[IntegerRange])
    extends IntegerDomain
{

    for range <- ranges do {
        require(! range.isEmpty)
    }
    for i <- 1 until ranges.size do {
        require(safeInc(ranges(i - 1).ub.value) < ranges(i).lb.value)
    }

    inline def ==(that: IntegerRangeList): Boolean = this.eq(that) || this.ranges == that.ranges
    inline def !=(that: IntegerRangeList): Boolean = ! (this == that)

    override def toString = if isEmpty then "{}" else ranges.iterator.map(_.toString).mkString(" ∪ ")

    inline override def isEmpty = ranges.isEmpty
    override lazy val size = ranges.iterator.map(_.size).foldLeft(0)(safeAdd(_, _))
    override def isComplete = ranges.size == 1 && ranges.head.isComplete
    override def isFinite = isEmpty || (ranges.head.lb.ne(null) && ranges.last.ub.ne(null))
    override def hasGaps = ranges.size > 1
    override def isBounded = isEmpty || (ranges.head.lb.ne(null) || ranges.last.ub.ne(null))
    override def lb = if isEmpty then One else ranges.head.lb
    override def ub = if isEmpty then Zero else ranges.last.ub
    override def hull = if ranges.size == 1 then ranges.head else IntegerRange(lb, ub)
    override def values = {
        require(isFinite)
        ranges.view.flatMap(_.values)
    }
    override def valuesIterator = {
        require(isFinite)
        ranges.iterator.flatMap(_.valuesIterator)
    }
    override def singleValue = {
        require(isSingleton)
        lb
    }
    override def contains(a: IntegerValue) = findIndexOfContainingRange(a, 0, ranges.size - 1) >= 0

    private lazy val rangeDistribution: Distribution = {
        val result = Distribution(ranges.size)
        for i <- ranges.indices do {
            result.setFrequency(i, ranges(i).size)
        }
        result
    }

    override def randomValue(randomGenerator: RandomGenerator) = {
        require(! isEmpty)
        if ranges.size == 1
        then ranges.head.randomValue(randomGenerator)
        else ranges(rangeDistribution.nextIndex(randomGenerator)).randomValue(randomGenerator)
    }

    override def nextRandomValue(randomGenerator: RandomGenerator, currentValue: IntegerValue) = {
        require(! isEmpty)
        if isSingleton
        then singleValue
        else if size == 2
        then if currentValue == lb then ub else lb
        else if ranges.size == 1
        then ranges.head.nextRandomValue(randomGenerator, currentValue)
        else {
            val i = findIndexOfContainingRange(currentValue, 0, ranges.size - 1)
            assert(i >= 0)
            try {
                rangeDistribution.addFrequencyDelta(i, -1)
                val j = rangeDistribution.nextIndex(randomGenerator)
                val range = ranges(j)
                if range.contains(currentValue)
                then range.nextRandomValue(randomGenerator, currentValue)
                else range.randomValue(randomGenerator)
            } finally {
                rangeDistribution.addFrequencyDelta(i, +1)
            }
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

    def isSubsetOf(that: IntegerRangeList): Boolean = {
        val lhs = this
        val rhs = that
        if lhs.isEmpty
        then true
        else if ! lhs.hasLb
        then IntegerRangeList.isSubsetOf(lhs, 0, rhs, 0)
        else {
            val i = rhs.findIndexOfContainingRange(lhs.lb, 0, rhs.ranges.size - 1)
            i >= 0 && IntegerRangeList.isSubsetOf(lhs, 0, rhs, i)
        }
    }

    def intersect(that: IntegerRangeList): IntegerDomain = {
        if this.cannotIntersect(that)
        then EmptyIntegerRange
        else {
            val i = this.findIndexOfFirstIntersectingRange(that.hull, 0, this.ranges.size - 1)
            if i < 0
            then EmptyIntegerRange
            else {
                val buf = new mutable.ArrayBuffer[IntegerRange](max(this.ranges.size, that.ranges.size))
                IntegerRangeList.intersect(this, i, that, 0, buf)
                IntegerDomain(buf)
            }
        }
    }

    def maybeIntersectionSize(that: IntegerRangeList): Option[Int] = {
        if this.cannotIntersect(that)
        then Some(0)
        else {
            val i = this.findIndexOfFirstIntersectingRange(that.hull, 0, this.ranges.size - 1)
            if i < 0 then Some(0) else IntegerRangeList.maybeIntersectionSize(this, i, that, 0, 0)
        }
    }

    def intersects(that: IntegerRangeList): Boolean = {
        if this.cannotIntersect(that)
        then false
        else {
            val i = this.findIndexOfFirstIntersectingRange(that.hull, 0, this.ranges.size - 1)
            if i < 0 then false else IntegerRangeList.intersects(this, i, that, 0)
        }
    }

    override def distanceTo(a: IntegerValue): IntegerValue = {
        require(! isEmpty)
        val lb = this.lb
        if lb.ne(null) && a < lb
        then lb - a
        else {
            val ub = this.ub
            if ub.ne(null) && a > ub
            then a - ub
            else if ranges.size == 1
            then Zero
            else {
                val i = findIndexOfContainingHole(a, 0, ranges.size - 2)
                if i < 0
                then Zero
                else IntegerValue(min(safeSub(a.value, ranges(i).ub.value), safeSub(ranges(i + 1).lb.value, a.value)))
            }
        }
    }

    def diff(that: IntegerRangeList): IntegerDomain = {
        val lhs = this
        val rhs = that
        if lhs.cannotIntersect(rhs)
        then lhs
        else {
            val buf = new mutable.ArrayBuffer[IntegerRange](lhs.ranges.size * 2)
            IntegerRangeList.diff(lhs, 0, rhs, 0, buf)
            IntegerDomain(buf)
        }
    }

    def union(that: IntegerRangeList): IntegerDomain = {
        val buf = new mutable.ArrayBuffer[IntegerRange](this.ranges.size + that.ranges.size)
        IntegerRangeList.union(this, 0, that, 0, buf)
        IntegerDomain(buf)
    }

    override def randomSubrange(randomGenerator: RandomGenerator): IntegerRange = {
        require(isFinite)
        if isEmpty
        then EmptyIntegerRange
        else {
            val numberOfSubrangesDistribution = new ArrayBackedDistribution(ranges.size)
            for i <- ranges.indices do {
                numberOfSubrangesDistribution.setFrequency(i, safeMul(ranges(i).size, ranges(i).size + 1) / 2)
            }
            ranges(numberOfSubrangesDistribution.nextIndex(randomGenerator)).randomSubrange(randomGenerator)
        }
    }

    override def mirrored: IntegerRangeList =
        if isEmpty then this else new IntegerRangeList(ranges.reverseIterator.map(_.mirrored).toVector)

    private def cannotIntersect(that: IntegerRangeList): Boolean =
        this.isEmpty ||
        that.isEmpty ||
        that.ranges.last.precedes(this.ranges.head) ||
        this.ranges.last.precedes(that.ranges.head)

    @tailrec
    private def findIndexOfFirstIntersectingRange(r: IntegerRange, start: Int, end: Int): Int =
        if start > end
        then -1
        else {
            val mid = start + (end - start + 1) / 2
            if ranges(mid).intersects(r)
            then if mid > 0 && ranges(mid - 1).intersects(r)
                 then findIndexOfFirstIntersectingRange(r, start, mid - 1)
                 else mid
            else if r.precedes(ranges(mid))
            then findIndexOfFirstIntersectingRange(r, start, mid - 1)
            else findIndexOfFirstIntersectingRange(r, mid + 1, end)
        }

    @tailrec
    private def findIndexOfContainingRange(a: IntegerValue, start: Int, end: Int): Int =
        if start > end
        then -1
        else {
            val mid = start + (end - start + 1) / 2
            if ranges(mid).contains(a)
            then mid
            else if mid > 0 && ranges(mid).lb > a
            then findIndexOfContainingRange(a, start, mid - 1)
            else findIndexOfContainingRange(a, mid + 1, end)
        }

    @tailrec
    private def findIndexOfContainingHole(a: IntegerValue, start: Int, end: Int): Int =
        if start > end
        then -1
        else {
            val mid = start + (end - start + 1) / 2
            if ranges(mid).hasUb && ranges(mid).ub < a && a < ranges(mid + 1).lb
            then mid
            else if ranges(mid).hasLb && ranges(mid).lb > a
            then findIndexOfContainingHole(a, start, mid - 1)
            else findIndexOfContainingHole(a, mid + 1, end)
        }

}

object IntegerRangeList {

    /**
     * Creates an IntegerRangeList instance from the given ranges.
     *
     * Tries to avoid memory allocation by re-using existing objects.
     */
    def apply(ranges: immutable.IndexedSeq[IntegerRange]) =
        if ranges.isEmpty then EmptyIntegerRangeList else new IntegerRangeList(ranges)

    /**
     * Creates an IntegerRangeList instance from the given range.
     *
     * Tries to avoid memory allocation by re-using existing objects.
     */
    def apply(range: IntegerRange) =
        if range.isEmpty then EmptyIntegerRangeList else new IntegerRangeList(Vector(range))

    /**
     * Creates an IntegerRangeList instance from the given boundaries.
     *
     * Tries to avoid memory allocation by re-using existing objects.
     */
    def apply(a: IntegerValue, b: IntegerValue) =
        if a.ne(null) && b.ne(null) && b < a
        then EmptyIntegerRangeList
        else new IntegerRangeList(Vector(IntegerRange(a, b)))

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
    private def isSubsetOf(lhs: IntegerRangeList, i: Int, rhs: IntegerRangeList, j: Int): Boolean =
        if i == lhs.ranges.size
        then true
        else if j == rhs.ranges.size
        then false
        else if lhs.ranges(i).precedes(rhs.ranges(j))
        then false
        else if lhs.ranges(i).isSubsetOf(rhs.ranges(j))
        then isSubsetOf(lhs, i + 1, rhs, j)
        else isSubsetOf(lhs, i, rhs, j + 1)

    @tailrec
    private def intersect(lhs: IntegerRangeList, i: Int, rhs: IntegerRangeList, j: Int, buf: mutable.Buffer[IntegerRange]): Unit = {
        if i == lhs.ranges.size then {
        } else if j == rhs.ranges.size then {
        } else {
            val r = lhs.ranges(i)
            val u = rhs.ranges(j)
            if r.precedes(u)
            then intersect(lhs, i + 1, rhs, j, buf)
            else if u.precedes(r)
            then intersect(lhs, i, rhs, j + 1, buf)
            else {
               buf += r.intersect(u)
               intersect(
                   lhs, if u.endsBefore(r) then i else i + 1,
                   rhs, if r.endsBefore(u) then j else j + 1,
                   buf)
            }
        }
    }

    @tailrec
    private def maybeIntersectionSize(lhs: IntegerRangeList, i: Int, rhs: IntegerRangeList, j: Int, n: Int): Option[Int] =
        if i == lhs.ranges.size
        then Some(n)
        else if j == rhs.ranges.size
        then Some(n)
        else {
            val r = lhs.ranges(i)
            val u = rhs.ranges(j)
            if r.precedes(u)
            then maybeIntersectionSize(lhs, i + 1, rhs, j, n)
            else if u.precedes(r)
            then maybeIntersectionSize(lhs, i, rhs, j + 1, n)
            else {
                val maybeM = r.maybeIntersectionSize(u)
                if maybeM.isEmpty
                then None
                else
                    maybeIntersectionSize(
                        lhs, if u.endsBefore(r) then i else i + 1,
                        rhs, if r.endsBefore(u) then j else j + 1,
                        n + maybeM.get)
            }
        }

    @tailrec
    private def intersects(lhs: IntegerRangeList, i: Int, rhs: IntegerRangeList, j: Int): Boolean =
        if i == lhs.ranges.size
        then false
        else if j == rhs.ranges.size
        then false
        else {
            val r = lhs.ranges(i)
            val u = rhs.ranges(j)
            if r.precedes(u)
            then intersects(lhs, i + 1, rhs, j)
            else if u.precedes(r)
            then intersects(lhs, i, rhs, j + 1)
            else
               r.intersects(u) ||
               intersects(
                   lhs, if u.endsBefore(r) then i else i + 1,
                   rhs, if r.endsBefore(u) then j else j + 1)
        }

    @tailrec
    private def diff(lhs: IntegerRangeList, i: Int, rhs: IntegerRangeList, j: Int, buf: mutable.Buffer[IntegerRange]): Unit = {
        if i != lhs.ranges.size then {
            val k = diff(lhs.ranges(i), rhs, j, buf)
            diff(lhs, i + 1, rhs, k, buf)
        }
    }

    @tailrec
    private def diff(lhs: IntegerRange, rhs: IntegerRangeList, j: Int, buf: mutable.Buffer[IntegerRange]): Int = {
        if j == rhs.ranges.size then {
            buf += lhs
            j
        } else {
            // r \ u
            val r = lhs
            val u = rhs.ranges(j)
            if u.precedes(r) then {
                diff(r, rhs, j + 1, buf)
            }
            else if r.precedes(u) then {
                buf += r
                j
            }
            else if u.endsBefore(r) then {
                if u.startsAfter(r) then {
                    buf += IntegerRange(r.lb, u.lb - One)
                }
                diff(IntegerRange(u.ub + One, r.ub), rhs, j + 1, buf)
            }
            else if u.startsAfter(r) then {
                buf += IntegerRange(r.lb, u.lb - One)
                j
            }
            else {
                assert(r.isSubsetOf(u))
                j
            }
        }
    }

    @tailrec
    private def union(lhs: IntegerRangeList, i: Int, rhs: IntegerRangeList, j: Int, buf: mutable.Buffer[IntegerRange]): Unit = {
        def addRange(u: IntegerRange): Unit = {
            if buf.isEmpty then {
                buf += u
            } else {
                val r = buf.last
                assert(! r.startsAfter(u))
                if r.precedesImmediately(u) || r.intersects(u) then {
                    buf.update(buf.size - 1, IntegerRange(r.lb, if r.endsAfter(u) then r.ub else u.ub))
                } else {
                    buf += u
                }
            }
        }
        if i == lhs.ranges.size then {
            if j < rhs.ranges.size then {
                addRange(rhs.ranges(j))
                union(lhs, i, rhs, j + 1, buf)
            }
        }
        else if j == rhs.ranges.size then {
            addRange(lhs.ranges(i))
            union(lhs, i + 1, rhs, j, buf)
        } else {
            val r = lhs.ranges(i)
            val u = rhs.ranges(j)
            if u.startsAfter(r) then {
                addRange(r)
                union(lhs, i + 1, rhs, j, buf)
            } else {
                addRange(u)
                union(lhs, i, rhs, j + 1, buf)
            }
        }
    }

}
