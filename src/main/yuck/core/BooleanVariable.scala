package yuck.core

/**
 * Implements Boolean variables.
 *
 * @author Michael Marte
 */
final class BooleanVariable
    (id: Id[AnyVariable], name: String, override protected var currentDomain: BooleanDomain)
    extends OrderedVariable[BooleanValue](id, name)
    with DomainHolder[BooleanValue, BooleanDomain]
{
    override protected def thisVariable = this
    override protected def intersectCurrentDomainWith(domain: BooleanDomain) = currentDomain.intersect(domain)
    override def domain: BooleanDomain = currentDomain
    override def pruneDomain(restriction: Domain[BooleanValue]) = pruneDomainImpl(restriction.asInstanceOf[BooleanDomain])
    override def relaxDomain(relaxation: Domain[BooleanValue]) = relaxDomainImpl(relaxation.asInstanceOf[BooleanDomain])
}
