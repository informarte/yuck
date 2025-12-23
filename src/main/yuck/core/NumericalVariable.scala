package yuck.core

/**
 * Provides an interface for working with numerical variables.
 */
abstract class NumericalVariable
    [A <: NumericalValue[A], D <: NumericalDomain[A, D], X <: NumericalVariable[A, D, X]]
    (id: Id[AnyVariable], name: String)
    extends OrderedVariable[A, D, X](id, name)
