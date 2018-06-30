package yuck

import scala.language.implicitConversions

/**
 * @author Michael Marte
 *
 */
package object core {

    @inline def safeAdd(a: Int, b: Int): Int = java.lang.Math.addExact(a, b)
    @inline def safeSub(a: Int, b: Int): Int = java.lang.Math.subtractExact(a, b)
    @inline def safeMul(a: Int, b: Int): Int = java.lang.Math.multiplyExact(a, b)
    @inline def safeInc(a: Int): Int = java.lang.Math.incrementExact(a)
    @inline def safeDec(a: Int): Int = java.lang.Math.decrementExact(a)
    @inline def safeNeg(a: Int): Int = java.lang.Math.negateExact(a)

    @inline def safeAdd(a: Long, b: Long): Long = java.lang.Math.addExact(a, b)
    @inline def safeSub(a: Long, b: Long): Long = java.lang.Math.subtractExact(a, b)
    @inline def safeMul(a: Long, b: Long): Long = java.lang.Math.multiplyExact(a, b)
    @inline def safeInc(a: Long): Long = java.lang.Math.incrementExact(a)
    @inline def safeDec(a: Long): Long = java.lang.Math.decrementExact(a)
    @inline def safeNeg(a: Long): Long = java.lang.Math.negateExact(a)
    @inline def safeToInt(a: Long): Int = java.lang.Math.toIntExact(a)

    type Costs = AnyValue

    val True = BooleanValue.get(0)
    val False = BooleanValue.get(1)
    val False2 = BooleanValue.get(2)
    val False3 = BooleanValue.get(3)
    val False4 = BooleanValue.get(4)
    val False5 = BooleanValue.get(5)
    val False6 = BooleanValue.get(6)
    val False7 = BooleanValue.get(7)
    val False8 = BooleanValue.get(8)
    val False9 = BooleanValue.get(9)
    val False10 = BooleanValue.get(10)

    val EmptyBooleanDomain = new BooleanDecisionDomain(false, false)
    val CompleteBooleanDecisionDomain = new BooleanDecisionDomain(true, true)
    val FalseDomain = new BooleanDecisionDomain(true, false)
    val TrueDomain = new BooleanDecisionDomain(false, true)
    val CompleteBooleanDomain = BooleanChannelDomain

    val MinusOne = IntegerValue.get(-1)
    val Zero = IntegerValue.get(0)
    val One = IntegerValue.get(1)
    val Two = IntegerValue.get(2)
    val Three = IntegerValue.get(3)
    val Four = IntegerValue.get(4)
    val Five = IntegerValue.get(5)
    val Six = IntegerValue.get(6)
    val Seven = IntegerValue.get(7)
    val Eight = IntegerValue.get(8)
    val Nine = IntegerValue.get(9)
    val Ten = IntegerValue.get(10)

    val EmptyIntegerRange = new IntegerRange(One, Zero)
    val CompleteIntegerRange = new IntegerRange(null, null)
    val NegativeIntegerRange = new IntegerRange(null, MinusOne)
    val NonNegativeIntegerRange = new IntegerRange(Zero, null)
    val PositiveIntegerRange = new IntegerRange(One, null)
    val NonPositiveIntegerRange = new IntegerRange(null, Zero)
    val ZeroToZeroIntegerRange = new IntegerRange(Zero, Zero)
    val OneToOneIntegerRange = new IntegerRange(One, One)
    val ZeroToOneIntegerRange = new IntegerRange(Zero, One)

    val EmptyIntegerRangeList = new IntegerRangeList()

    val EmptyIntegerSetValue = new IntegerSetValue(EmptyIntegerRange)
    val CompleteIntegerSetValue = new IntegerSetValue(CompleteIntegerRange)

    val CompleteIntegerSetDomain = new IntegerPowersetDomain(CompleteIntegerRange)

    implicit def createLexicographicOrderingForTraversableOnce[T](implicit ord: Ordering[T]): Ordering[TraversableOnce[T]] =
        new Ordering[TraversableOnce[T]] {
            override def compare(t: TraversableOnce[T], u: TraversableOnce[T]) = {
                val i = t.toIterator
                val j = u.toIterator
                var result = 0
                while (result == 0 && i.hasNext && j.hasNext) {
                    result = ord.compare(i.next, j.next)
                }
                if (result == 0) {
                    result = Ordering.Boolean.compare(i.hasNext, j.hasNext)
                }
                result
            }
        }

    val DEFAULT_SEED = 5489 // Boost Mersenne Twister default seed

    val DEFAULT_RESTART_LIMIT = 100

}
