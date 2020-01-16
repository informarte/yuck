package yuck.core

/**
 * Implements the immutable Boolean channel domain.
 *
 * @author Michael Marte
 */
object BooleanChannelDomain extends BooleanDomain {
    override def toString = "{%s, %s, %s, ...}".format(True, False, False2)
    override def size = !!!
    override def isComplete = true
    override def isFinite = false
    override def isEmpty = false
    override def isSingleton = false
    override def isBounded = true
    override def hasLb = true
    override def hasUb = false
    override def lb = True
    override def ub = null
    override def values = !!!
    override def singleValue = !!!
    override def contains(a: BooleanValue) = true
    override def randomValue(randomGenerator: RandomGenerator) = !!!
    override def nextRandomValue(randomGenerator: RandomGenerator, currentValue: BooleanValue) = !!!
    override def boundFromBelow(lb: BooleanValue) = !!!
    override def boundFromAbove(ub: BooleanValue) = !!!
    override def bisect = !!!
    override def mirrored = !!!
}
