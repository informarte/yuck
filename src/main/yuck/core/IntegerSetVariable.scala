package yuck.core

/**
 * Implements integer-set variables.
 *
 * @author Michael Marte
 */
final class IntegerSetVariable
    (id: Id[AnyVariable], name: String, var currentDomain: IntegerSetDomain)
    extends OrderedVariable[IntegerSetValue](id, name)
{
    override def domain: IntegerSetDomain = currentDomain
    override protected def setDomain(domain: Domain[IntegerSetValue]) = {
        currentDomain = domain.asInstanceOf[IntegerSetDomain]
    }
}
