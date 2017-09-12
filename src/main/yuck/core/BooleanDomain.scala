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
    override def equals(that: Domain[BooleanValue]) = this.equals(that.asInstanceOf[BooleanDomain])
    def equals(that: BooleanDomain): Boolean =
        this.eq(that) || (this.containsFalse == that.containsFalse && this.containsTrue == that.containsTrue)
    override def compare(that: Domain[BooleanValue]): Int =
        BooleanDomain.ordering.compare(this, that.asInstanceOf[BooleanDomain])
    override def valueTraits = BooleanValueTraits
    override def size = (if (containsFalse) 1 else 0) + (if (containsTrue) 1 else 0)
    override def isComplete = containsFalse && containsTrue
    override def isFinite = true
    override def isBounded = true
    override def hasLb = true
    override def hasUb = true
    override def lb = if (containsFalse) False else True
    override def ub = if (containsTrue) True else False
    override def hull = this
    override def values =
        if (containsFalse && containsTrue) List(False, True)
        else if (containsFalse) List(False)
        else if (containsTrue) List(True)
        else Nil
    override def singleValue = {
        require(isSingleton)
        if (containsFalse) False else True
    }
    override def contains(a: BooleanValue) = if (a.value) containsTrue else containsFalse
    override def randomValue(randomGenerator: RandomGenerator) = {
        require(! isEmpty)
        if (isSingleton) {
            if (containsFalse) False else True
        } else {
            if (randomGenerator.nextDecision) True else False
        }
    }
    override def nextRandomValue(randomGenerator: RandomGenerator, currentValue: BooleanValue) = {
        require(! isEmpty)
        if (isSingleton) singleValue else if (currentValue.value) False else True
    }
    override def isSubsetOf(that: Domain[BooleanValue]) =
        this.isSubsetOf(that.asInstanceOf[BooleanDomain])
    def isSubsetOf(that: BooleanDomain): Boolean =
        (! this.containsFalse || that.containsFalse) && (! this.containsTrue || that.containsTrue)
    override def intersects(that: Domain[BooleanValue]) =
        this.intersects(that.asInstanceOf[BooleanDomain])
    def intersects(that: BooleanDomain): Boolean =
        (this.containsFalse && that.containsFalse) || (this.containsTrue && that.containsTrue)
    override def intersect(that: Domain[BooleanValue]): BooleanDomain =
        this.intersect(that.asInstanceOf[BooleanDomain])
    def intersect(that: BooleanDomain): BooleanDomain =
        new BooleanDomain(this.containsFalse && that.containsFalse, this.containsTrue && that.containsTrue)
    override def union(that: Domain[BooleanValue]): BooleanDomain =
        this.union(that.asInstanceOf[BooleanDomain])
    def union(that: BooleanDomain): BooleanDomain =
        new BooleanDomain(this.containsFalse || that.containsFalse, this.containsTrue || that.containsTrue)
    override def diff(that: Domain[BooleanValue]): BooleanDomain =
        this.diff(that.asInstanceOf[BooleanDomain])
    def diff(that: BooleanDomain): BooleanDomain =
        new BooleanDomain(this.containsFalse && ! that.containsFalse, this.containsTrue && ! that.containsTrue)
    override def symdiff(that: Domain[BooleanValue]): BooleanDomain =
        super.symdiff(that).asInstanceOf[BooleanDomain]
}

/**
 * Provides tools for the implementation of BooleanDomain.
 *
 * @author Michael Marte
 */
final object BooleanDomain {
    /** {}  < {false} < {true} < {false, true} */
    val ordering = new Ordering[BooleanDomain] {
        override def compare(lhs: BooleanDomain, rhs: BooleanDomain) =
            if (lhs.equals(rhs)) 0
            else if (lhs.isEmpty) -1
            else if (lhs.containsFalse && !lhs.containsTrue && rhs.containsTrue) -1
            else if (!lhs.containsFalse && lhs.containsTrue && rhs.containsFalse && rhs.containsTrue) -1
            else +1
    }
}
