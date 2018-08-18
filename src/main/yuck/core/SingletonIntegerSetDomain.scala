package yuck.core

/**
 * Turns a given integer domain into an immutable singleton integer-set domain.
 *
 * @author Michael Marte
 */
final class SingletonIntegerSetDomain
    (val base: IntegerDomain)
    extends IntegerSetDomain
{
    override def toString = "{%s}".format(base.toString)
    def equals(that: SingletonIntegerSetDomain): Boolean =
        this.eq(that) || this.base.eq(that.base) || this.base == that.base
    override def hashCode = base.hashCode
    override def size = 1
    override def isEmpty = false
    override def isComplete = false
    override def isFinite = true
    override def isBounded = true
    override def lb = singleValue
    override def ub = singleValue
    override def hull = this
    override def values = List(singleValue)
    override def singleValue = new IntegerSetValue(base)
    override def contains(a: IntegerSetValue) = a.set == base
    override def randomValue(randomGenerator: RandomGenerator) = singleValue
    override def nextRandomValue(randomGenerator: RandomGenerator, currentValue: IntegerSetValue) = singleValue
    def isSubsetOf(that: SingletonIntegerSetDomain): Boolean = this == that
    def intersects(that: SingletonIntegerSetDomain): Boolean = this == that
}
