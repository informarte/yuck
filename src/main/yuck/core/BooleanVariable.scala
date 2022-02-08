package yuck.core

/**
 * Implements Boolean variables.
 *
 * @author Michael Marte
 */
final class BooleanVariable
    (id: Id[AnyVariable], name: String, var currentDomain: BooleanDomain)
    extends OrderedVariable[BooleanValue](id, name)
{
    inline override def domain: BooleanDomain = currentDomain
    override protected def setDomain(domain: Domain[BooleanValue]) = {
        currentDomain = domain.asInstanceOf[BooleanDomain]
    }
}
