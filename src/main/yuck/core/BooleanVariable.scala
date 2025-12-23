package yuck.core

/**
 * Implements Boolean variables.
 */
final class BooleanVariable
    (id: Id[AnyVariable], name: String, var currentDomain: BooleanDomain)
    extends OrderedVariable[BooleanValue, BooleanDomain, BooleanVariable](id, name)
{
    inline override def domain: BooleanDomain = currentDomain
    override protected def setDomain(domain: BooleanDomain) = {
        currentDomain = domain
    }
}
