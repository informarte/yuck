package yuck.core

/**
 * Implements the empty integer-set domain.
 *
 * @author Michael Marte
 */
object EmptyIntegerSetDomain extends IntegerSetDomain {
    override def isBounded = true
    override val lb = CompleteIntegerSetValue
    override val ub = EmptyIntegerSetValue
    override def hull = this
    override def values = None
    override def singleValue = {
        require(false)
        ???
    }
    override def contains(a: IntegerSetValue) = false
    override def randomValue(randomGenerator: RandomGenerator) = {
        require(false)
        ???
    }
    override def nextRandomValue(randomGenerator: RandomGenerator, currentValue: IntegerSetValue) = {
        require(false)
        ???
    }
    override def size = 0
    override def isComplete = false
    override def isFinite = true
}
