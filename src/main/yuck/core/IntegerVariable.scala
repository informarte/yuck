package yuck.core

/**
 * Implements integer variables.
 *
 * @author Michael Marte
 */
final class IntegerVariable
    (id: Id[AnyVariable], name: String, override protected var currentDomain: IntegerDomain)
    extends NumericalVariable[IntegerValue](id, name)
    with DomainHolder[IntegerValue, IntegerDomain]
{
    override protected def thisVariable = this
    override protected def intersectCurrentDomainWith(domain: IntegerDomain) = currentDomain.intersect(domain)
    override def domain: IntegerDomain = currentDomain
    override def pruneDomain(restriction: Domain[IntegerValue]) = pruneDomainImpl(restriction.asInstanceOf[IntegerDomain])
    override def relaxDomain(relaxation: Domain[IntegerValue]) = relaxDomainImpl(relaxation.asInstanceOf[IntegerDomain])
}
