package yuck.core

/**
 * Provides properties of numerical types.
 */
abstract class NumericalTypeTraits
    [A <: NumericalValue[A], D <: NumericalDomain[A, D], X <: NumericalVariable[A, D, X]]
    extends OrderedTypeTraits[A, D, X]
{

    override val domainPruner: NumericalDomainPruner[A, D]

    /** Integrates Yuck values with the Scala library. */
    val numericalOperations: Numeric[A]

    /** The additive identity. */
    val zero: A

    /** The multiplicative identity. */
    val one: A

    /** The smallest representable value. */
    val minValue: A

    /** The greatest representable value. */
    val maxValue: A

    /** The domain that contains all non-negative values. */
    val nonNegativeDomain: D

}
