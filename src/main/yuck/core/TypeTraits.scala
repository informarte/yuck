package yuck.core

import scala.collection.*

/**
 * Provides general type properties.
 *
 * CAUTION!!!
 * Do not use `x.isInstanceOf[Variable[V]]` or `x.asInstanceOf[Variable[V]]`!
 * It will compile but not work due to type erasure :-(
 * Instead use type traits for type checking and down-casting!
 */
abstract class TypeTraits[A <: Value[A], D <: Domain[A, D], X <: Variable[A, D, X]] {

    /** The class of the concrete values. */
    val valueClass: Class[A]

    /** The class of the concrete domains. */
    val domainClass: Class[D]

    /** The class of the concrete variables. */
    val variableClass: Class[X]

    /** Informs about which operations are fully implemented. */
    val domainCapabilities: DomainCapabilities

    /** Computes the normal form of the given value. */
    def normalizedValue(a: A): A

    /** Creates a domain from the given value set. */
    def createDomain(values: Set[A]): D

    /** The empty domain over V. */
    val emptyDomain: D

    /** The largest domain over V. */
    val completeDomain: D

    /** The standard cost model for generic constraints over V. */
    val costModel: EqualityCostModel[A]

    /** The standard domain pruner for generic constraints over V. */
    val domainPruner: DomainPruner[A, D]

    /** Creates a variable over the given domain in the given space. */
    def createVariable(space: Space, name: String, domain: D): X

    /** Creates a channel variable in the given space. */
    def createChannel(space: Space): X

    /** Tries to cast the given value to V. */
    def safeDowncast(a: AnyValue): A

    /** Tries to cast the given domain to D. */
    def safeDowncast(d: AnyDomain): D

    /** Tries to cast the given variable to X. */
    def safeDowncast(x: AnyVariable): X

}
