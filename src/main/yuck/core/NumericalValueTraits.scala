package yuck.core

import scala.collection._

/**
 * Provides properties of numerical values.
 *
 * @author Michael Marte
 */
abstract class NumericalValueTraits[Value <: NumericalValue[Value]] extends OrderedValueTraits[Value] {

    override def createDomain(values: Set[Value]): NumericalDomain[Value]
    override val emptyDomain: NumericalDomain[Value]
    override val completeDomain: NumericalDomain[Value]
    override val domainPruner: NumericalDomainPruner[Value]
    override def createVariable(space: Space, name: String, domain: Domain[Value]): NumericalVariable[Value]
    override def createChannel(space: Space): NumericalVariable[Value]
    override def safeDowncast(x: AnyDomain): NumericalDomain[Value]
    override def safeDowncast(x: AnyVariable): NumericalVariable[Value]

    /** Integrates Yuck values with the Scala library. */
    val numericalOperations: Numeric[Value]

    /** Returns the additive identity. */
    val zero: Value

    /** Returns the multiplicative identity. */
    val one: Value

    /** Returns the smallest representable value. */
    val minValue: Value

    /** Returns the greatest representable value. */
    val maxValue: Value

    /** Returns the domain that contains all non-negative values. */
    val nonNegativeDomain: NumericalDomain[Value]

}
