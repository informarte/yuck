package yuck.core

import scala.collection.*

/**
 * Provides general value properties.
 *
 * CAUTION!!!
 * Do not use `x.isInstanceOf[Variable[V]]` or `x.asInstanceOf[Variable[V]]`!
 * It will compile but not work due to type erasure :-(
 * Instead use value traits for type checking and down-casting!
 *
 * @author Michael Marte
 */
abstract class ValueTraits[V <: Value[V]] {

    /** The type of the concrete values. */
    val valueType: Class[V]

    /** Informs about which operations are fully implemented. */
    val domainCapabilities: DomainCapabilities

    /** Computes the normal form of the given value. */
    def normalizedValue(a: V): V

    /** Creates a domain from the given value set. */
    def createDomain(values: Set[V]): Domain[V]

    /** The empty domain over V. */
    val emptyDomain: Domain[V]

    /** The largest domain over V. */
    val completeDomain: Domain[V]

    /** The standard cost model for generic constraints over V. */
    val costModel: EqualityCostModel[V]

    /** The standard domain pruner for generic constraints over V. */
    val domainPruner: DomainPruner[V]

    /** Creates a variable over the given domain in the given space. */
    def createVariable(space: Space, name: String, domain: Domain[V]): Variable[V]

    /** Creates a channel variable in the given space. */
    def createChannel(space: Space): Variable[V]

    /** Tries to cast the given value to V. */
    def safeDowncast(a: AnyValue): V

    /** Tries to cast the given domain to a domain over V. */
    def safeDowncast(d: AnyDomain): Domain[V]

    /** Tries to cast the given variable to a variable over V. */
    def safeDowncast(x: AnyVariable): Variable[V]

}
