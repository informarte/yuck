package yuck

import scala.collection.immutable

/**
 * @author Michael Marte
 *
 */
package object core {

    inline def safeAdd(a: Int, b: Int): Int = java.lang.Math.addExact(a, b)
    inline def safeSub(a: Int, b: Int): Int = java.lang.Math.subtractExact(a, b)
    inline def safeMul(a: Int, b: Int): Int = java.lang.Math.multiplyExact(a, b)
    inline def safeInc(a: Int): Int = java.lang.Math.incrementExact(a)
    inline def safeDec(a: Int): Int = java.lang.Math.decrementExact(a)
    inline def safeNeg(a: Int): Int = java.lang.Math.negateExact(a)
    inline def min(a: Int, b: Int): Int = java.lang.Math.min(a, b)
    inline def max(a: Int, b: Int): Int = java.lang.Math.max(a, b)
    inline def abs(a: Int): Int = java.lang.Math.abs(a)

    inline def safeAdd(a: Long, b: Long): Long = java.lang.Math.addExact(a, b)
    inline def safeSub(a: Long, b: Long): Long = java.lang.Math.subtractExact(a, b)
    inline def safeMul(a: Long, b: Long): Long = java.lang.Math.multiplyExact(a, b)
    inline def safeInc(a: Long): Long = java.lang.Math.incrementExact(a)
    inline def safeDec(a: Long): Long = java.lang.Math.decrementExact(a)
    inline def safeNeg(a: Long): Long = java.lang.Math.negateExact(a)
    inline def safeToInt(a: Long): Int = java.lang.Math.toIntExact(a)
    inline def min(a: Long, b: Long): Long = java.lang.Math.min(a, b)
    inline def max(a: Long, b: Long): Long = java.lang.Math.max(a, b)
    inline def abs(a: Long): Long = java.lang.Math.abs(a)

    inline def ld(a: Double): Double = java.lang.Math.log(a) / java.lang.Math.log(2.0)

    type Costs = AnyValue

    val True = BooleanValue(0)
    val False = BooleanValue(1)
    val False2 = BooleanValue(2)
    val False3 = BooleanValue(3)
    val False4 = BooleanValue(4)
    val False5 = BooleanValue(5)
    val False6 = BooleanValue(6)
    val False7 = BooleanValue(7)
    val False8 = BooleanValue(8)
    val False9 = BooleanValue(9)
    val False10 = BooleanValue(10)

    val EmptyBooleanDomain = new BooleanDomain(false, false)
    val CompleteBooleanDomain = new BooleanDomain(true, true)
    val FalseDomain = new BooleanDomain(true, false)
    val TrueDomain = new BooleanDomain(false, true)

    val MinusOne = IntegerValue(-1)
    val Zero = IntegerValue(0)
    val One = IntegerValue(1)

    val EmptyIntegerRange = new IntegerRange(One, Zero)
    val CompleteIntegerRange = new IntegerRange(null, null)
    val NegativeIntegerRange = new IntegerRange(null, MinusOne)
    val NonNegativeIntegerRange = new IntegerRange(Zero, null)
    val PositiveIntegerRange = new IntegerRange(One, null)
    val NonPositiveIntegerRange = new IntegerRange(null, Zero)

    val EmptyIntegerRangeList = new IntegerRangeList(immutable.IndexedSeq[IntegerRange]())

    val EmptyBitSet = new SixtyFourBitSet(0L)
    val FullBitSet = new SixtyFourBitSet(SixtyFourBitSet.MaxUInt)

    val EmptyIntegerSetValue = new IntegerSetValue(EmptyIntegerRange)
    val CompleteIntegerSetValue = new IntegerSetValue(CompleteIntegerRange)

    val CompleteIntegerSetDomain = new IntegerPowersetDomain(CompleteIntegerRange)

    given lexicographicOrderingForIterator[T](using ord: Ordering[T]): Ordering[Iterator[T]] with {
        override def compare(i: Iterator[T], j: Iterator[T]) = {
            var result = 0
            while (result == 0 && i.hasNext && j.hasNext) {
                result = ord.compare(i.next(), j.next())
            }
            if (result == 0) {
                result = Ordering.Boolean.compare(i.hasNext, j.hasNext)
            }
            result
        }
    }

    given lexicographicOrderingForIterable[T](using ord: Ordering[T]): Ordering[Iterable[T]] with {
        val iteratorOrd = lexicographicOrderingForIterator(using ord)
        override def compare(u: Iterable[T], v: Iterable[T]) =
            iteratorOrd.compare(u.iterator, v.iterator)
    }

    val DefaultSeed = 0x0a23d679a633c596L

    // The scala package object provides List and Vector but it does not provide map and set implementations.
    export scala.collection.immutable.{HashMap, TreeMap, HashSet, TreeSet}

}
