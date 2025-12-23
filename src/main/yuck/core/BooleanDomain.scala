package yuck.core

/**
 * Implements immutable Boolean domains with two values: True and False (with violation 1).
 */
final class BooleanDomain
    (val containsFalse: Boolean, val containsTrue: Boolean)
    extends OrderedDomain[BooleanValue, BooleanDomain]
{

    override def valueType = classOf[BooleanValue]

    override def hashCode = 3 * (3 + containsFalse.hashCode) + containsTrue.hashCode

    /** {}  < {false} < {true} < {false, true} */
    override def compare(that: BooleanDomain) = {
        val lhs = this
        val rhs = that
        if lhs == rhs
        then 0
        else if lhs.isEmpty
        then -1
        else if lhs.containsFalse && !lhs.containsTrue && rhs.containsTrue
        then -1
        else if !lhs.containsFalse && lhs.containsTrue && rhs.containsFalse && rhs.containsTrue
        then -1
        else +1
    }

    inline override def ==(that: BooleanDomain): Boolean =
        this.eq(that) || (this.containsFalse == that.containsFalse && this.containsTrue == that.containsTrue)

    override def size = (if containsFalse then 1 else 0) + (if containsTrue then 1 else 0)
    override def isComplete = false
    override def isFinite = true
    override def isBounded = true
    override def hasLb = true
    override def hasUb = true

    override def lb = if containsTrue then True else False
    override def ub = if containsFalse then False else True
    override def hull: BooleanDomain = this

    override def values =
        if containsFalse && containsTrue
        then BooleanDomain.ListWithFalseAndTrue
        else if containsFalse
        then BooleanDomain.ListWithFalse
        else if containsTrue
        then BooleanDomain.ListWithTrue
        else Nil

    override def singleValue = {
        require(isSingleton)
        if containsFalse then False else True
    }

    override def contains(a: BooleanValue) =
        if a.truthValue then containsTrue else containsFalse

    override def randomValue(randomGenerator: RandomGenerator) = {
        require(! isEmpty)
        if isSingleton then {
            if containsFalse then False else True
        } else {
            if randomGenerator.nextDecision() then True else False
        }
    }

    override def nextRandomValue(randomGenerator: RandomGenerator, currentValue: BooleanValue) = {
        require(! isEmpty)
        if isSingleton then singleValue else if currentValue.truthValue then False else True
    }

    override def randomSubdomain(randomGenerator: RandomGenerator): BooleanDomain = ???

    override def isSubsetOf(that: BooleanDomain): Boolean =
        (! this.containsFalse || that.containsFalse) && (! this.containsTrue || that.containsTrue)
    override def intersects(that: BooleanDomain): Boolean =
        (this.containsFalse && that.containsFalse) || (this.containsTrue && that.containsTrue)
    override def intersect(that: BooleanDomain): BooleanDomain =
        BooleanDomain(this.containsFalse && that.containsFalse, this.containsTrue && that.containsTrue)
    override def union(that: BooleanDomain): BooleanDomain =
        BooleanDomain(this.containsFalse || that.containsFalse, this.containsTrue || that.containsTrue)
    override def diff(that: BooleanDomain): BooleanDomain =
        BooleanDomain(this.containsFalse && ! that.containsFalse, this.containsTrue && ! that.containsTrue)

}

object BooleanDomain {

    given Ordering[BooleanDomain] = BooleanDomainOrdering

    private val ListWithFalseAndTrue = List(False, True)
    private val ListWithFalse = List(False)
    private val ListWithTrue = List(True)

    /**
     * Creates a Boolean decision domain.
     *
     * Tries to avoid memory allocation by re-using existing objects
     */
    def apply(containsFalse: Boolean, containsTrue: Boolean): BooleanDomain = {
        (containsFalse, containsTrue) match {
            case (false, false) => EmptyBooleanDomain
            case (false, true) => TrueDomain
            case (true, false) => FalseDomain
            case (true, true) => CompleteBooleanDomain
        }
    }

    /**
     * Creates a Boolean decision domain.
     *
     * Tries to avoid memory allocation by re-using existing objects
     */
    def apply(lb: BooleanValue, ub: BooleanValue): BooleanDomain = {
        (lb.violation, ub.violation) match {
            case (1, 1) => FalseDomain
            case (1, 0) => EmptyBooleanDomain
            case (0, 1) => CompleteBooleanDomain
            case (0, 0) => TrueDomain
            case _ => ???
        }
    }

}
