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
    val UnboundedBooleanDomain = new BooleanDomain(true, true)

    val MinusThree = IntegerValue.get(-3)
    val MinusTwo = IntegerValue.get(-2)
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
    val Eleven = IntegerValue.get(11)

    val EmptyIntegerDomain = new IntegerDomain()
    val UnboundedIntegerDomain = new IntegerDomain(null, null)
    val NegativeIntegerDomain = new IntegerDomain(null, MinusOne)
    val NonNegativeIntegerDomain = new IntegerDomain(Zero, null)
    val PositiveIntegerDomain = new IntegerDomain(One, null)
    val ZeroIntegerDomain = new IntegerDomain(Zero)
    val OneIntegerDomain = new IntegerDomain(One)
    val ZeroOneIntegerDomain = new IntegerDomain(Zero, One)

    val EmptyIntegerSet = new IntegerSetValue(EmptyIntegerDomain)
    val UnboundedIntegerSet = new IntegerSetValue(UnboundedIntegerDomain)
    val NonNegativeIntegerSet = new IntegerSetValue(NonNegativeIntegerDomain)

    val UnboundedIntegerSetDomain = new IntegerPowersetDomain(UnboundedIntegerDomain)

    implicit def createOrderingForTraversableOnce[T](implicit ord: Ordering[T]): Ordering[TraversableOnce[T]] =
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
