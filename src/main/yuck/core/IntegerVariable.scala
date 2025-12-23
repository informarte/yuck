package yuck.core

/**
 * Implements integer variables.
 */
final class IntegerVariable
    (id: Id[AnyVariable], name: String, var currentDomain: IntegerDomain)
    extends NumericalVariable[IntegerValue, IntegerDomain, IntegerVariable](id, name)
{
    inline override def domain: IntegerDomain = currentDomain
    override protected def setDomain(domain: IntegerDomain) = {
        currentDomain = domain
    }
}
