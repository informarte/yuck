package yuck.core

import scala.collection.*

/**
 * Provides properties of numerical values.
 *
 * @author Michael Marte
 */
abstract class NumericalValueTraits[V <: NumericalValue[V]] extends OrderedValueTraits[V] {

    override def createDomain(values: Set[V]): NumericalDomain[V]
    override val emptyDomain: NumericalDomain[V]
    override val completeDomain: NumericalDomain[V]
    override val domainPruner: NumericalDomainPruner[V]
    override def createVariable(space: Space, name: String, domain: Domain[V]): NumericalVariable[V]
    override def createChannel(space: Space): NumericalVariable[V]
    override def safeDowncast(x: AnyDomain): NumericalDomain[V]
    override def safeDowncast(x: AnyVariable): NumericalVariable[V]

    /** Integrates Yuck values with the Scala library. */
    val numericalOperations: Numeric[V]

    /** Returns the additive identity. */
    val zero: V

    /** Returns the multiplicative identity. */
    val one: V

    /** Returns the smallest representable value. */
    val minValue: V

    /** Returns the greatest representable value. */
    val maxValue: V

    /** Returns the domain that contains all non-negative values. */
    val nonNegativeDomain: NumericalDomain[V]

}
