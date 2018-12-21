package yuck.core

/**
 * Provides an interface for working with ordered variables.
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
