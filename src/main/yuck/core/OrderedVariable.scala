package yuck.core

/**
 * Provides an interface for working with variables that have an ordered domain.
 *
 * @author Michael Marte
 */
abstract class OrderedVariable
    [Value <: OrderedValue[Value]]
    (id: Id[AnyVariable], name: String)
    extends Variable[Value](id, name)
{
    override def domain: OrderedDomain[Value]
}
