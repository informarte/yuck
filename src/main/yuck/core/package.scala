package yuck

import scala.language.implicitConversions

/**
 * @author Michael Marte
 *
 */
package object core {

    type Costs = AnyValue

    val False = new BooleanValue(false)
    val True = new BooleanValue(true)

    val EmptyBooleanDomain = new BooleanDomain(false, false)
    val CompleteBooleanDomain = new BooleanDomain(true, true)
    val FalseDomain = new BooleanDomain(true, false)
    val TrueDomain = new BooleanDomain(false, true)

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
