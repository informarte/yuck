package yuck.core

import scala.collection._

/**
 * Provides the power set of a given integer domain as immutable integer-set domain.
 *
 * @author Michael Marte
 */
final class IntegerPowersetDomain(
    override val base: IntegerDomain)
    extends IntegerSetDomain
{
    final override def toString = "P(%s)".format(base.toString)
    override def size = {
        require(base.size < 31)
        1 << base.size
    }
    override def isEmpty = false
    override def isSingleton = base.isEmpty
    override def isFinite = base.isFinite
    override def contains(a: IntegerSetValue) = a.set.isSubsetOf(base)
    override def values = {
        require(isFinite)
        base.values.toSet.subsets.map(values => new IntegerSetValue(new IntegerDomain(values)))
    }
    override def singleValue = {
        require(isSingleton)
        new IntegerSetValue(base)
    }
    override def randomValue(randomGenerator: RandomGenerator) = {
        require(isFinite)
        val values = new mutable.HashSet[IntegerValue]
        base.values.foreach(a => if (randomGenerator.nextDecision) values += a)
        val a = new IntegerSetValue(new IntegerDomain(values))
        a
    }
    override def nextRandomValue(randomGenerator: RandomGenerator, currentValue: IntegerSetValue) = {
        require(isFinite)
        require(! isSingleton)
        val a = base.randomValue(randomGenerator)
        if (currentValue.set.contains(a)) {
            val b = new IntegerSetValue(currentValue.set.subtract(new IntegerDomain(a)))
            new IntegerSetValue(currentValue.set.subtract(new IntegerDomain(a)))
        } else {
            val b = new IntegerSetValue(currentValue.set.unite(new IntegerDomain(a)))
            new IntegerSetValue(currentValue.set.unite(new IntegerDomain(a)))
        }
    }
    override def isBounded = base.isBounded
    override def lb = EmptyIntegerSet
    override def ub = new IntegerSetValue(base)
    override def hull = this
}
