package yuck.core

/**
 * Provides an interface for working with numerical variables.
 *
 * @author Michael Marte
 */
abstract class NumericalVariable
    [Value <: NumericalValue[Value]]
    (id: Id[AnyVariable], name: String)
    extends OrderedVariable[Value](id, name)
{
    override def domain: NumericalDomain[Value]
}
