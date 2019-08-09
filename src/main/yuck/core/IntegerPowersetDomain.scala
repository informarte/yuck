package yuck.core

import scala.collection._

/**
 * Provides the power set of a given integer domain as immutable integer-set domain.
 *
 * @author Michael Marte
 */
final class IntegerPowersetDomain
    (val base: IntegerDomain)
    extends IntegerSetDomain
{
    override def toString = "P(%s)".format(base.toString)
    def equals(that: IntegerPowersetDomain): Boolean =
        this.eq(that) || this.base.eq(that.base) || this.base == that.base
    override def hashCode = base.hashCode
    override def size = {
        require(base.size < 31)
        1 << base.size
    }
    override def isComplete = base.isComplete
    override def isFinite = base.isFinite
    override def isEmpty = false
    override def isSingleton = base.isEmpty
    override def isBounded = base.isBounded
    override def lb = EmptyIntegerSetValue
    override def ub = new IntegerSetValue(base)
    override def hull = this
    override def values =
        base.values.toSet.subsets.to(LazyList).map(values => new IntegerSetValue(IntegerDomain.createDomain(values)))
    override def singleValue = {
        require(isSingleton)
        new IntegerSetValue(base)
    }
    override def contains(a: IntegerSetValue) = a.set.isSubsetOf(base)
    override def randomValue(randomGenerator: RandomGenerator) =
        new IntegerSetValue(base.randomSubdomain(randomGenerator))
    override def nextRandomValue(randomGenerator: RandomGenerator, currentValue: IntegerSetValue) = {
        require(! isSingleton)
        val a = base.randomValue(randomGenerator)
        if (currentValue.set.contains(a)) {
            new IntegerSetValue(currentValue.set.diff(IntegerDomain.createRange(a, a)))
        } else {
            new IntegerSetValue(currentValue.set.union(IntegerDomain.createRange(a, a)))
        }
    }
    def isSubsetOf(that: IntegerPowersetDomain): Boolean =
        this.base.isSubsetOf(that.base)
    def intersects(that: IntegerPowersetDomain): Boolean =
        this.base.intersects(that.base)
    def intersect(that: IntegerPowersetDomain): IntegerPowersetDomain =
        new IntegerPowersetDomain(this.base.intersect(that.base))
    def union(that: IntegerPowersetDomain): IntegerPowersetDomain =
        !!!
    def diff(that: IntegerPowersetDomain): IntegerPowersetDomain =
        new IntegerPowersetDomain(this.base.diff(that.base))
}
