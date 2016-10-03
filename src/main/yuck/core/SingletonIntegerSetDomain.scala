package yuck.core

/**
 * Turns a given integer domain into an immutable singleton integer-set domain.
 *
 * @author Michael Marte
 */
final class SingletonIntegerSetDomain(
    override val base: IntegerDomain)
    extends IntegerSetDomain
{
    final override def toString = "{%s}".format(base.toString)
    override def size = 1
    override def isEmpty = false
    override def isFinite = true
    override def contains(a: IntegerSetValue) = a.set == base
    override def singleValue = new IntegerSetValue(base)
    override def values = List(singleValue)
    override def randomValue(randomGenerator: RandomGenerator) = singleValue
    override def nextRandomValue(randomGenerator: RandomGenerator, currentValue: IntegerSetValue) = ???
    override def isBounded = true
    override def lb = singleValue
    override def ub = singleValue
    override def hull = this
    def isSubsetOf(rhs: SingletonIntegerSetDomain): Boolean = false
}
