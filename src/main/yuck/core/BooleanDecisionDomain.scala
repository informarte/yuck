package yuck.core

/**
 * Implements immutable Boolean domains with two values: True and False (with violation 1).
 *
 * @author Michael Marte
 */
final class BooleanDecisionDomain
    (val containsFalse: Boolean, val containsTrue: Boolean)
    extends BooleanDomain
{
    import BooleanDecisionDomain.createDomain
    override def hashCode = 3 * (3 + containsFalse.hashCode) + containsTrue.hashCode
    def equals(that: BooleanDecisionDomain): Boolean =
        this.eq(that) || (this.containsFalse == that.containsFalse && this.containsTrue == that.containsTrue)
    override def size = (if (containsFalse) 1 else 0) + (if (containsTrue) 1 else 0)
    override def isComplete = false
    override def isFinite = true
    override def isBounded = true
    override def hasLb = true
    override def hasUb = true
    override def lb = if (containsTrue) True else False
    override def ub = if (containsFalse) False else True
    override def values =
        if (containsFalse && containsTrue) BooleanDecisionDomain.listWithFalseAndTrue
        else if (containsFalse) BooleanDecisionDomain.listWithFalse
        else if (containsTrue) BooleanDecisionDomain.listWithTrue
        else Nil
    override def singleValue = {
        require(isSingleton)
        if (containsFalse) False else True
    }
    override def contains(a: BooleanValue) =
        if (a.truthValue) containsTrue else containsFalse && a.violation == 1
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
        if (isSingleton) singleValue else if (currentValue.truthValue) False else True
    }
    def isSubsetOf(that: BooleanDecisionDomain): Boolean =
        (! this.containsFalse || that.containsFalse) && (! this.containsTrue || that.containsTrue)
    def intersects(that: BooleanDecisionDomain): Boolean =
        (this.containsFalse && that.containsFalse) || (this.containsTrue && that.containsTrue)
    def intersect(that: BooleanDecisionDomain): BooleanDecisionDomain =
        createDomain(this.containsFalse && that.containsFalse, this.containsTrue && that.containsTrue)
    def union(that: BooleanDecisionDomain): BooleanDecisionDomain =
        createDomain(this.containsFalse || that.containsFalse, this.containsTrue || that.containsTrue)
    def diff(that: BooleanDecisionDomain): BooleanDecisionDomain =
        createDomain(this.containsFalse && ! that.containsFalse, this.containsTrue && ! that.containsTrue)
    override def boundFromBelow(lb: BooleanValue) = ???
    override def boundFromAbove(ub: BooleanValue) = ???
    override def bisect = ???
}

/**
 * Companion object to BooleanDecisionDomain.
 *
 * @author Michael Marte
 */
final object BooleanDecisionDomain {

    private val listWithFalseAndTrue = List(False, True)
    private val listWithFalse = List(False)
    private val listWithTrue = List(True)

    /** {}  < {false} < {true} < {false, true} */
    val ordering = new Ordering[BooleanDecisionDomain] {
        override def compare(lhs: BooleanDecisionDomain, rhs: BooleanDecisionDomain) =
            if (lhs.equals(rhs)) 0
            else if (lhs.isEmpty) -1
            else if (lhs.containsFalse && ! lhs.containsTrue && rhs.containsTrue) -1
            else if (! lhs.containsFalse && lhs.containsTrue && rhs.containsFalse && rhs.containsTrue) -1
            else +1
    }

    /**
     * Creates a Boolean decision domain.
     *
     * Tries to avoid memory allocation by re-using existing objects
     */
    def createDomain(containsFalse: Boolean, containsTrue: Boolean): BooleanDecisionDomain = {
        (containsFalse, containsTrue) match {
            case (false, false) => EmptyBooleanDomain
            case (false, true) => TrueDomain
            case (true, false) => FalseDomain
            case (true, true) => CompleteBooleanDecisionDomain
        }
    }

    /**
     * Creates a Boolean decision domain.
     *
     * Tries to avoid memory allocation by re-using existing objects
     */
    def createDomain(lb: BooleanValue, ub: BooleanValue): BooleanDecisionDomain = {
        (lb, ub) match {
            case (False, False) => FalseDomain
            case (False, True) => EmptyBooleanDomain
            case (True, False) => CompleteBooleanDecisionDomain
            case (True, True) => TrueDomain
            case _ => ???
        }
    }

}
