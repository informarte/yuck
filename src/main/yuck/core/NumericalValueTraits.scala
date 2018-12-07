package yuck.core

/**
 * Provides properties of numerical values.
 *
 * @author Michael Marte
 */
abstract class NumericalValueTraits[Value <: NumericalValue[Value]] extends OrderedValueTraits[Value] {

    override val emptyDomain: NumericalDomain[Value]
    override val completeDomain: NumericalDomain[Value]
    override val domainPruner: NumericalDomainPruner[Value]

    /** Casts the given domain to a numerical domain over Value. */
    override def safeDowncast(x: Domain[Value]): NumericalDomain[Value] =
        x.asInstanceOf[NumericalDomain[Value]]

    /** Returns the additive identity. */
    val zero: Value

    /** Returns the multiplicative identity. */
    val one: Value

    /** Returns the domain that contains all non-negative values. */
    val nonNegativeDomain: Domain[Value]

}
