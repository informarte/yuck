package yuck.core

/**
 * Domain pruner interface for use by generic constraints.
 *
 * @author Michael Marte
 */
abstract class DomainPruner[Value <: AnyValue] {

    protected val valueTraits: ValueTraits[Value]

    def eqRule
        (lhs: Domain[Value], rhs: Domain[Value]):
        (Domain[Value], Domain[Value]) =
        (lhs, rhs)

    def neRule
        (lhs: Domain[Value], rhs: Domain[Value]):
        (Domain[Value], Domain[Value]) =
        (lhs, rhs)

}
