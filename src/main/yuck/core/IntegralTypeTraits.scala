package yuck.core

/**
 * Provides properties of integral types.
 */
abstract class IntegralTypeTraits
    [A <: IntegralValue[A], D <: NumericalDomain[A, D], X <: NumericalVariable[A, D, X]]
    extends NumericalTypeTraits[A, D, X]
{

    override val numericalOperations: Integral[A]

}
