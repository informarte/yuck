package yuck.core

/**
 * Implements integer variables.
 *
 * @author Michael Marte
 */
final class IntegerVariable
    (id: Id[AnyVariable], name: String, var currentDomain: IntegerDomain)
    extends NumericalVariable[IntegerValue](id, name)
{
    override def domain: IntegerDomain = currentDomain
    override protected def setDomain(domain: Domain[IntegerValue]) = {
        currentDomain = domain.asInstanceOf[IntegerDomain]
    }
}
