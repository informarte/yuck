package yuck.core

/**
 * Implements integer-set variables.
 *
 * @author Michael Marte
 */
final class IntegerSetVariable
    (id: Id[AnyVariable], name: String, override protected var currentDomain: IntegerSetDomain)
    extends OrderedVariable[IntegerSetValue](id, name)
    with DomainHolder[IntegerSetValue, IntegerSetDomain]
{
    override protected def thisVariable = this
    override protected def intersectCurrentDomainWith(domain: IntegerSetDomain) = currentDomain.intersect(domain)
    override def domain: IntegerSetDomain = currentDomain
    override def pruneDomain(restriction: Domain[IntegerSetValue]) = pruneDomainImpl(restriction.asInstanceOf[IntegerSetDomain])
    override def relaxDomain(relaxation: Domain[IntegerSetValue]) = relaxDomainImpl(relaxation.asInstanceOf[IntegerSetDomain])
}
