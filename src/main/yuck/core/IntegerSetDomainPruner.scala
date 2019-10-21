package yuck.core

/**
 * Provides methods for pruning integer-set domains.
 *
 * @author Michael Marte
 */
object IntegerSetDomainPruner extends OrderedDomainPruner[IntegerSetValue] {

    override protected val valueTraits = IntegerSetValueTraits

    override def eq
        (lhs: Domain[IntegerSetValue], rhs: Domain[IntegerSetValue]):
        (IntegerSetDomain, IntegerSetDomain) =
    {
        val intersection = lhs.asInstanceOf[IntegerSetDomain].intersect(rhs)
        (intersection, intersection)
    }

}
