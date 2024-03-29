package yuck.core

import scala.collection.*

/**
 * Provides the power set of a given integer domain as immutable integer-set domain.
 *
 * @author Michael Marte
 */
final class IntegerPowersetDomain
    (val base: IntegerDomain)
    extends IntegerSetDomain
{
    override def hashCode = base.hashCode
    override def toString = "P(%s)".format(base.toString)
    def ==(that: IntegerPowersetDomain): Boolean =
        this.eq(that) || this.base.eq(that.base) || this.base == that.base
    inline def !=(that: IntegerPowersetDomain): Boolean = ! (this == that)
    override def size = {
        require(base.size < 31)
        1 << base.size
    }
    inline override def isComplete = base.isComplete
    inline override def isFinite = base.isFinite
    override def isEmpty = false
    inline override def isSingleton = base.isEmpty
    inline override def isBounded = base.isBounded
    override def lb = EmptyIntegerSetValue
    override def ub = new IntegerSetValue(base)
    override def hull = this
    override def values = valuesIterator.to(LazyList)
    override def valuesIterator =
        base.values.toSet.subsets().map(values => new IntegerSetValue(IntegerDomain(values)))
    override def singleValue = {
        require(isSingleton)
        new IntegerSetValue(base)
    }
    inline override def contains(a: IntegerSetValue) = a.set.isSubsetOf(base)
    override def randomValue(randomGenerator: RandomGenerator) =
        new IntegerSetValue(base.randomSubdomain(randomGenerator))
    override def nextRandomValue(randomGenerator: RandomGenerator, currentValue: IntegerSetValue) = {
        require(! isSingleton)
        val a = base.randomValue(randomGenerator)
        if (currentValue.set.contains(a)) {
            new IntegerSetValue(currentValue.set.diff(IntegerRange(a, a)))
        } else {
            new IntegerSetValue(currentValue.set.union(IntegerRange(a, a)))
        }
    }
    inline def isSubsetOf(that: IntegerPowersetDomain): Boolean =
        this.base.isSubsetOf(that.base)
    inline def intersects(that: IntegerPowersetDomain): Boolean =
        this.base.intersects(that.base)
    inline def intersect(that: IntegerPowersetDomain): IntegerPowersetDomain =
        new IntegerPowersetDomain(this.base.intersect(that.base))
    inline def union(that: IntegerPowersetDomain): IntegerPowersetDomain =
        ???
    inline def diff(that: IntegerPowersetDomain): IntegerPowersetDomain =
        new IntegerPowersetDomain(this.base.diff(that.base))
}
