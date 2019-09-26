package yuck.core

/**
 * Provides properties of integral values.
 *
 * @author Michael Marte
 */
abstract class IntegralValueTraits[Value <: IntegralValue[Value]] extends NumericalValueTraits[Value] {

    override val numericalOperations: Integral[Value]

}
