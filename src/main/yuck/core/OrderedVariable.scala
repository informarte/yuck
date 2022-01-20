package yuck.core

/**
 * Provides an interface for working with variables that have an ordered domain.
 *
 * @author Michael Marte
 */
abstract class OrderedVariable
    [V <: OrderedValue[V]]
    (id: Id[AnyVariable], name: String)
    extends Variable[V](id, name)
{
    override def domain: OrderedDomain[V]
}
