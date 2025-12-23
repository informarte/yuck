package yuck.core

/**
 * Provides an interface for working with variables that have an ordered domain.
 */
abstract class OrderedVariable
    [A <: OrderedValue[A], D <: OrderedDomain[A, D], X <: OrderedVariable[A, D, X]]
    (id: Id[AnyVariable], name: String)
    extends Variable[A, D, X](id, name)
{
    override def domain: D
}
