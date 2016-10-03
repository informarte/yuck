package yuck.core

/**
 * Implements immutable Boolean domains.
 *
 * @author Michael Marte
 */
final class BooleanDomain
    (val containsFalse: Boolean, val containsTrue: Boolean)
    extends OrderedDomain[BooleanValue]
{
    override def hashCode = 3 * (3 + containsFalse.hashCode) + containsTrue.hashCode
    override def equals(that: Any) = that match {
        case rhs: BooleanDomain => {
            val lhs = this
            lhs.eq(rhs) ||
            (lhs.containsFalse == rhs.containsFalse && lhs.containsTrue == rhs.containsTrue)
        }
        case _ => false
    }
    override def valueTraits = BooleanValue.Traits
    override def size = (if (containsFalse) 1 else 0) + (if (containsTrue) 1 else 0)
    override def isFinite = true
    override def values =
        if (containsFalse && containsTrue) List(False, True)
        else if (containsFalse) List(False)
        else if (containsTrue) List(True)
        else Nil
    override def contains(a: BooleanValue) = if (a.value) containsTrue else containsFalse
    override def singleValue = {
        require(isSingleton)
        if (containsFalse) False else True
    }
    override def randomValue(randomGenerator: RandomGenerator) = {
        require(! isEmpty)
        if (isSingleton) {
            if (containsFalse) False else True
        } else {
            if (randomGenerator.nextDecision) True else False
        }
    }
    override def nextRandomValue(randomGenerator: RandomGenerator, currentValue: BooleanValue) = {
        require(size > 1)
        if (currentValue.value) False else True
    }
    override def isBounded = true
    override def lb = if (containsFalse) False else True
    override def ub = if (containsTrue) True else False
    override def hull = this
    /** Decides whether this is a subset of that. */
    def isSubsetOf(that: BooleanDomain): Boolean = this.values.forall(a => that.contains(a))
}
