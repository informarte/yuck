package yuck.core

import scala.language.higherKinds

/**
 * Provides general value properties.
 *
 * CAUTION!!!
 * Do not use x.isInstanceOf[Variable[Value]] or x.asInstanceOf[Variable[Value]]!
 * It will compile but not work due to type erasure :-(
 * Instead use value traits for type checking and down-casting!
 *
 * @author Michael Marte
 */
abstract class ValueTraits[Value <: AnyValue] {

    /** Returns the type of the concrete values. */
    val valueType: Class[Value]

    /** Creates a domain from the given value set. */
    def createDomain(values: Set[Value]): Domain[Value]

    /** Returns the empty domain over Value. */
    val emptyDomain: Domain[Value]

    /** Returns the largest domain over Value. */
    val completeDomain: Domain[Value]

    /** Returns a domain pruner for use by generic constraints. */
    val domainPruner: DomainPruner[Value]

    /** Creates a variable over the given domain in the given space. */
    def createVariable(space: Space, name: String, domain: Domain[Value]): Variable[Value]

    /** Creates a channel variable in the given space. */
    def createChannel(space: Space): Variable[Value]

    /** Tries to cast the given value to Value. */
    def safeDowncast(a: AnyValue): Value

    /** Tries to cast the given domain to a domain over Value. */
    def safeDowncast(d: AnyDomain): Domain[Value]

    /** Tries to cast the given variable to a variable over Value. */
    def safeDowncast(x: AnyVariable): Variable[Value]

}
