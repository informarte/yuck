package yuck.core

/**
 * Provides an interface for working with numerical variables.
 *
 * @author Michael Marte
 */
abstract class NumericalVariable
    [V <: NumericalValue[V]]
    (id: Id[AnyVariable], name: String)
    extends OrderedVariable[V](id, name)
{
    override def domain: NumericalDomain[V]
}
