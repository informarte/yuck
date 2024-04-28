package yuck.core

/**
 * Implements immutable Boolean domains with two values: True and False (with violation 1).
 *
 * @author Michael Marte
 */
final class BooleanDomain
    (val containsFalse: Boolean, val containsTrue: Boolean)
    extends OrderedDomain[BooleanValue]
{

    override def valueType = classOf[BooleanValue]

    override def hashCode = 3 * (3 + containsFalse.hashCode) + containsTrue.hashCode

    /** {}  < {false} < {true} < {false, true} */
    override def compare(that: OrderedDomain[BooleanValue]) = {
        val lhs = this
        val rhs = that.asInstanceOf[BooleanDomain]
        if (lhs == rhs) 0
        else if (lhs.isEmpty) -1
        else if (lhs.containsFalse && !lhs.containsTrue && rhs.containsTrue) -1
        else if (!lhs.containsFalse && lhs.containsTrue && rhs.containsFalse && rhs.containsTrue) -1
        else +1
    }

    inline override def ==(that: Domain[BooleanValue]): Boolean = this == that.asInstanceOf[BooleanDomain]
    inline def ==(that: BooleanDomain): Boolean =
        this.eq(that) || (this.containsFalse == that.containsFalse && this.containsTrue == that.containsTrue)
    inline def !=(that: BooleanDomain): Boolean = ! (this == that)

    override def size = (if (containsFalse) 1 else 0) + (if (containsTrue) 1 else 0)
    override def isComplete = false
    override def isFinite = true
    override def isBounded = true
    override def hasLb = true
    override def hasUb = true

    override def lb = if (containsTrue) True else False
    override def ub = if (containsFalse) False else True
    override def hull: BooleanDomain = this

    override def values =
        if (containsFalse && containsTrue) BooleanDomain.ListWithFalseAndTrue
        else if (containsFalse) BooleanDomain.ListWithFalse
        else if (containsTrue) BooleanDomain.ListWithTrue
        else Nil

    override def singleValue = {
        require(isSingleton)
        if (containsFalse) False else True
    }

    override def contains(a: BooleanValue) =
        if (a.truthValue) containsTrue else containsFalse

    override def randomValue(randomGenerator: RandomGenerator) = {
        require(! isEmpty)
        if (isSingleton) {
            if (containsFalse) False else True
        } else {
            if (randomGenerator.nextDecision()) True else False
        }
    }

    override def nextRandomValue(randomGenerator: RandomGenerator, currentValue: BooleanValue) = {
        require(! isEmpty)
        if (isSingleton) singleValue else if (currentValue.truthValue) False else True
    }

    override def randomSubdomain(randomGenerator: RandomGenerator): BooleanDomain = ???

    override def isSubsetOf(that: Domain[BooleanValue]): Boolean = this.isSubsetOf(that.asInstanceOf[BooleanDomain])
    override def intersects(that: Domain[BooleanValue]): Boolean = this.intersects(that.asInstanceOf[BooleanDomain])
    override def intersect(that: Domain[BooleanValue]): BooleanDomain = this.intersect(that.asInstanceOf[BooleanDomain])
    override def union(that: Domain[BooleanValue]): BooleanDomain = this.union(that.asInstanceOf[BooleanDomain])
    override def diff(that: Domain[BooleanValue]): BooleanDomain = this.diff(that.asInstanceOf[BooleanDomain])

    def isSubsetOf(that: BooleanDomain): Boolean =
        (! this.containsFalse || that.containsFalse) && (! this.containsTrue || that.containsTrue)
    def intersects(that: BooleanDomain): Boolean =
        (this.containsFalse && that.containsFalse) || (this.containsTrue && that.containsTrue)
    def intersect(that: BooleanDomain): BooleanDomain =
        BooleanDomain(this.containsFalse && that.containsFalse, this.containsTrue && that.containsTrue)
    def union(that: BooleanDomain): BooleanDomain =
        BooleanDomain(this.containsFalse || that.containsFalse, this.containsTrue || that.containsTrue)
    def diff(that: BooleanDomain): BooleanDomain =
        BooleanDomain(this.containsFalse && ! that.containsFalse, this.containsTrue && ! that.containsTrue)

}

/**
 * Companion object to BooleanDecisionDomain.
 *
 * @author Michael Marte
 */
object BooleanDomain {

    given ordering: Ordering[OrderedDomain[BooleanValue]] = BooleanDomainOrdering

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
