package yuck.core

import java.lang.Long.*

import scala.collection.mutable

import yuck.core.SixtyFourBitSet.ValueRange

/**
 * Implements immutable subsets of 0..63.
 *
 * @author Michael Marte
 */
final class SixtyFourBitSet(val set: Long) extends IntegerDomain {

    import SixtyFourBitSet.MaxUInt

    inline def ==(that: SixtyFourBitSet): Boolean = this.set == that.set
    inline def !=(that: SixtyFourBitSet): Boolean = this.set != that.set

    override def toString = "%s".format(IntegerDomain.ensureRangeList(this))

    inline override def size = bitCount(set)
    override def isComplete = false
    override def isFinite = true
    inline override def isEmpty = set == 0L
    inline override def isSingleton = size == 1
    override def hasGaps = numberOfLeadingZeros(set) + bitCount(set) + numberOfTrailingZeros(set) < 64
    override def isBounded = true
    override def hasLb = true
    override def lb = if (isEmpty) One else IntegerValue(numberOfTrailingZeros(set))
    override def hasUb = true
    override def ub = if (isEmpty) Zero else IntegerValue(63 - numberOfLeadingZeros(set))
    override def hull = IntegerRange(lb, ub)

    override def values = new Iterable[IntegerValue] {
        override def isEmpty = SixtyFourBitSet.this.isEmpty
        override def knownSize = SixtyFourBitSet.this.size
        override def head = if (isEmpty) throw new NoSuchElementException else SixtyFourBitSet.this.lb
        override def headOption = if (isEmpty) None else Some(SixtyFourBitSet.this.lb)
        override def last = if (isEmpty) throw new NoSuchElementException else SixtyFourBitSet.this.ub
        override def lastOption = if (isEmpty) None else Some(SixtyFourBitSet.this.ub)
        override def iterator = SixtyFourBitSet.this.valuesIterator
    }

    override def valuesIterator = new Iterator[IntegerValue]() {
        private var i = numberOfTrailingZeros(set)
        inline override def hasNext = i < 64
        override def next() = {
            if (! hasNext) {
                throw new NoSuchElementException
            }
            val a = IntegerValue(i)
            i += 1
            while (i < 64 && (set & (1L << i)) == 0) {
                i += 1
            }
            a
        }
        override def knownSize = SixtyFourBitSet.this.size
    }

    override def singleValue = {
        require(isSingleton)
        IntegerValue(numberOfTrailingZeros(set))
    }

    inline private def contains(i: Long) = 0 <= i && i < 64 && (set & (1L << i)) != 0
    inline override def contains(a: IntegerValue) = contains(a.value)

    override def distanceTo(a: IntegerValue): IntegerValue = {
        require(! isEmpty)
        val i = a.value
        if (contains(i)) Zero
        else {
            val lb = numberOfTrailingZeros(set).toLong
            val ub = 63 - numberOfLeadingZeros(set).toLong
            if (i < lb) IntegerValue(lb - i)
            else if (i > ub) IntegerValue(i - ub)
            else {
                var d = 1L
                while (! contains(i - d) && ! contains(i + d)) {
                    d += 1
                }
                IntegerValue(d)
            }
        }
    }

    inline override def randomValue(randomGenerator: RandomGenerator) =
        IntegerValue(SixtyFourBitSet.randomValue(set, randomGenerator))

    override def nextRandomValue(randomGenerator: RandomGenerator, currentValue: IntegerValue) = {
        require(! isEmpty)
        if (isSingleton) {
            singleValue
        } else if (size == 2) {
            if (currentValue == lb) ub else lb
        } else {
            require(ValueRange.contains(currentValue))
            IntegerValue(SixtyFourBitSet.randomValue(set & ~(1L << currentValue.value), randomGenerator))
        }
    }

    override def randomSubrange(randomGenerator: RandomGenerator): SixtyFourBitSet =
        SixtyFourBitSet(IntegerDomain.ensureRangeList(this).randomSubrange(randomGenerator))

    override def randomSubdomain(randomGenerator: RandomGenerator): SixtyFourBitSet = {
        var subset = 0L
        var i = numberOfTrailingZeros(set)
        val j = 63 - numberOfLeadingZeros(set)
        while (i <= j) {
            val mask = 1L << i
            if ((set & mask) != 0 && randomGenerator.nextDecision()) {
                subset = subset | mask
            }
            i += 1
        }
        SixtyFourBitSet(subset)
    }

    override def mirrored = IntegerDomain(values).mirrored

    override def boundFromBelow(lb: IntegerValue) =
        if (lb.value <= 0) this
        else if (lb.value >= 63) EmptyBitSet
        else SixtyFourBitSet(set & (MaxUInt << lb.value))

    override def boundFromAbove(ub: IntegerValue) =
        if (ub.value < 0) EmptyBitSet
        else if (ub.value >= 63) this
        else SixtyFourBitSet(set & (MaxUInt >>> (63 - ub.value)))

    override def bisect = {
        require(! isEmpty)
        val lb = numberOfTrailingZeros(set)
        val ub = 63 - numberOfLeadingZeros(set)
        val mid = lb + (ub - lb + 1) / 2
        (this.intersect(SixtyFourBitSet(lb, mid - 1)), this.intersect(SixtyFourBitSet(mid, ub)))
    }

    inline def isSubsetOf(that: SixtyFourBitSet): Boolean = (this.set | that.set) == that.set
    inline def intersects(that: SixtyFourBitSet): Boolean = (this.set & that.set) != 0
    inline def intersect(that: SixtyFourBitSet): SixtyFourBitSet = SixtyFourBitSet(this.set & that.set)
    inline def maybeIntersectionSize(that: SixtyFourBitSet): Option[Int] = Some(this.intersect(that).size)
    inline def union(that: SixtyFourBitSet): SixtyFourBitSet = SixtyFourBitSet(this.set | that.set)
    inline def diff(that: SixtyFourBitSet): SixtyFourBitSet = SixtyFourBitSet(this.set & ~that.set)

}

/**
 * Companion object to SixtyFourBitSet.
 *
 * @author Michael Marte
 */
object SixtyFourBitSet {

    val MaxUInt = 0xffffffffffffffffL
    val ValueRange = IntegerRange(0, 63)

    private def randomValue(set: Long, randomGenerator: RandomGenerator): Int = {
        require(set != 0)
        var i = numberOfTrailingZeros(set)
        var j = randomGenerator.nextInt(bitCount(set))
        while (j > 0) {
            i += 1
            if ((set & (1L << i)) != 0) {
                j -= 1
            }
        }
        i
    }

    /**
     * Creates a SixtyFourBitSet instance from the given bit mask.
     *
     * Tries to avoid memory allocation by re-using existing objects.
     */
    def apply(set: Long): SixtyFourBitSet =
        if (set == 0) EmptyBitSet
        else if (set == MaxUInt) FullBitSet
        else new SixtyFourBitSet(set)

    /**
     * Creates a SixtyFourBitSet instance from the given boundaries.
     *
     * Tries to avoid memory allocation by re-using existing objects.
     *
     * Throws if SixtyFourBitSet cannot house the input data.
     */
    def apply(lb: Long, ub: Long): SixtyFourBitSet = {
        require(lb >= 0 && ub < 64)
        apply(MaxUInt & (MaxUInt << lb) & (MaxUInt >>> (63 - ub)))
    }

    /**
     * Creates a SixtyFourBitSet instance from the given values.
     *
     * Tries to avoid memory allocation by re-using existing objects.
     *
     * Throws if SixtyFourBitSet cannot house the input data.
     */
    inline def apply(values: Iterable[Long]): SixtyFourBitSet = {
        var set = 0L
        for (a <- values) {
            require(a >= 0 && a < 64)
            set = set | (1L << a)
        }
        apply(set)
    }

    /**
     * Creates an SixtyFourBitSet instance from the given boundaries.
     *
     * Tries to avoid memory allocation by re-using existing objects.
     *
     * Throws if SixtyFourBitSet cannot house the input data.
     */
    inline def apply(lb: IntegerValue, ub: IntegerValue): SixtyFourBitSet =
        apply(lb.value, ub.value)

    /**
     * Creates a SixtyFourBitSet instance from the given integer domain.
     *
     * Tries to avoid memory allocation by re-using existing objects.
     *
     * Throws if SixtyFourBitSet cannot house the input data.
     */
    def apply(d: IntegerDomain): SixtyFourBitSet = d match {
        case bitSet: SixtyFourBitSet => bitSet
        case _ => apply(d.values.view.map(_.value))
    }

}
