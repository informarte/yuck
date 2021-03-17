package yuck

import scala.collection.immutable
import scala.language.implicitConversions

/**
 * @author Michael Marte
 *
 */
package object core {

    /**
     * `!!!` should be used for marking methods that cannot to be implemented.
     *
     * @throws IllegalArgumentException
     */
    def !!! : Nothing = throw new IllegalArgumentException("Implementation is not possible")

    @inline def safeAdd(a: Int, b: Int): Int = java.lang.Math.addExact(a, b)
    @inline def safeSub(a: Int, b: Int): Int = java.lang.Math.subtractExact(a, b)
    @inline def safeMul(a: Int, b: Int): Int = java.lang.Math.multiplyExact(a, b)
    @inline def safeInc(a: Int): Int = java.lang.Math.incrementExact(a)
    @inline def safeDec(a: Int): Int = java.lang.Math.decrementExact(a)
    @inline def safeNeg(a: Int): Int = java.lang.Math.negateExact(a)
    @inline def min(a: Int, b: Int): Int = java.lang.Math.min(a, b)
    @inline def max(a: Int, b: Int): Int = java.lang.Math.max(a, b)
    @inline def abs(a: Int): Int = java.lang.Math.abs(a)

    @inline def safeAdd(a: Long, b: Long): Long = java.lang.Math.addExact(a, b)
    @inline def safeSub(a: Long, b: Long): Long = java.lang.Math.subtractExact(a, b)
    @inline def safeMul(a: Long, b: Long): Long = java.lang.Math.multiplyExact(a, b)
    @inline def safeInc(a: Long): Long = java.lang.Math.incrementExact(a)
    @inline def safeDec(a: Long): Long = java.lang.Math.decrementExact(a)
    @inline def safeNeg(a: Long): Long = java.lang.Math.negateExact(a)
    @inline def safeToInt(a: Long): Int = java.lang.Math.toIntExact(a)
    @inline def min(a: Long, b: Long): Long = java.lang.Math.min(a, b)
    @inline def max(a: Long, b: Long): Long = java.lang.Math.max(a, b)
    @inline def abs(a: Long): Long = java.lang.Math.abs(a)

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

    val EmptyBooleanDomain = new BooleanDecisionDomain(false, false)
    val CompleteBooleanDecisionDomain = new BooleanDecisionDomain(true, true)
    val FalseDomain = new BooleanDecisionDomain(true, false)
    val TrueDomain = new BooleanDecisionDomain(false, true)
    val CompleteBooleanDomain = BooleanChannelDomain

    val MinusOne = IntegerValue(-1)
    val Zero = IntegerValue(0)
    val One = IntegerValue(1)
    val Two = IntegerValue(2)
    val Three = IntegerValue(3)
    val Four = IntegerValue(4)
    val Five = IntegerValue(5)
    val Six = IntegerValue(6)
    val Seven = IntegerValue(7)
    val Eight = IntegerValue(8)
    val Nine = IntegerValue(9)
    val Ten = IntegerValue(10)

    val EmptyIntegerRange = new IntegerRange(One, Zero)
    val CompleteIntegerRange = new IntegerRange(null, null)
    val NegativeIntegerRange = new IntegerRange(null, MinusOne)
    val NonNegativeIntegerRange = new IntegerRange(Zero, null)
    val PositiveIntegerRange = new IntegerRange(One, null)
    val NonPositiveIntegerRange = new IntegerRange(null, Zero)
    val ZeroToZeroIntegerRange = new IntegerRange(Zero, Zero)
    val OneToOneIntegerRange = new IntegerRange(One, One)
    val ZeroToOneIntegerRange = new IntegerRange(Zero, One)

    val EmptyIntegerRangeList = new IntegerRangeList(immutable.IndexedSeq[IntegerRange]())

    val EmptyIntegerSetValue = new IntegerSetValue(EmptyIntegerRange)
    val CompleteIntegerSetValue = new IntegerSetValue(CompleteIntegerRange)

    val CompleteIntegerSetDomain = new IntegerPowersetDomain(CompleteIntegerRange)

    implicit def createLexicographicOrderingForIterator[T](implicit ord: Ordering[T]): Ordering[Iterator[T]] =
        new Ordering[Iterator[T]] {
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

    implicit def createLexicographicOrderingForIterable[T](implicit ord: Ordering[T]): Ordering[Iterable[T]] =
        new Ordering[Iterable[T]] {
            val iteratorOrd = createLexicographicOrderingForIterator(ord)
            override def compare(u: Iterable[T], v: Iterable[T]) =
                iteratorOrd.compare(u.iterator, v.iterator)
        }

    val DefaultSeed = 0x0a23d679a633c596L

    val DefaultRestartLimit = 1000

}
