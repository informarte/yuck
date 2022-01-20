package yuck.core

/**
 * Provides properties of integral values.
 *
 * @author Michael Marte
 */
abstract class IntegralValueTraits[V <: IntegralValue[V]] extends NumericalValueTraits[V] {

    override val numericalOperations: Integral[V]

}
