package yuck.core

/**
 * Provides methods for pruning integer-set domains.
 */
object IntegerSetDomainPruner extends OrderedDomainPruner[IntegerSetValue, IntegerSetDomain] {

    override protected val typeTraits = IntegerSetTypeTraits

    override def eqRule
        (lhs: IntegerSetDomain, rhs: IntegerSetDomain):
        (IntegerSetDomain, IntegerSetDomain) =
    {
        val intersection = lhs.intersect(rhs)
        (intersection, intersection)
    }

}
