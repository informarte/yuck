package yuck.core

/**
 * Implements integer-set variables.
 */
final class IntegerSetVariable
    (id: Id[AnyVariable], name: String, var currentDomain: IntegerSetDomain)
    extends OrderedVariable[IntegerSetValue, IntegerSetDomain, IntegerSetVariable](id, name)
{
    inline override def domain: IntegerSetDomain = currentDomain
    override protected def setDomain(domain: IntegerSetDomain) = {
        currentDomain = domain
    }
}
