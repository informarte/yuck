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
abstract class AnyValueTraits[Value <: AnyValue] {

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

    /** Throws when the type of the given value differs from Value. */
    final def checkType(a: AnyValue) {
        require(
            a.valueType == valueType,
            "Value %s has type %s but not %s as expected".format(a, a.valueType, valueType))
    }

    /** Casts the given value to Value. */
    private final def unsafeDowncast(a: AnyValue): Value =
        a.asInstanceOf[Value]

    /** Tries to cast the given value to Value. */
    final def safeDowncast(a: AnyValue): Value = {
        checkType(a)
        unsafeDowncast(a)
    }

    /** Throws when the given domain is not a domain over Value. */
    final def checkType(d: AnyDomain) {
        require(
            d.valueType == valueType,
            "Domain %s has type %s but not %s as expected".format(d, d.valueType, valueType))
    }

    /** Casts the given domain to a domain over Value. */
    private final def unsafeDowncast(x: AnyDomain): Domain[Value] =
        x.asInstanceOf[Domain[Value]]

    /** Tries to cast the given domain to a domain over Value. */
    final def safeDowncast(d: AnyDomain): Domain[Value] = {
        checkType(d)
        unsafeDowncast(d)
    }

    /** Throws when the given variable is not a variable over Value. */
    final def checkType(x: AnyVariable) {
        require(
            x.valueType == valueType,
            "Variable %s has type %s but not %s as expected".format(x.name, x.valueType, valueType))
    }

    /** Casts the given variable to a variable over Value. */
    final def unsafeDowncast(x: AnyVariable): Variable[Value] =
        x.asInstanceOf[Variable[Value]]

    /** Casts the given variable collection to a collection of variables over Value. */
    final def unsafeDowncast[CC[X] <: TraversableOnce[X]](xs: CC[AnyVariable]): CC[Variable[Value]] =
        xs.asInstanceOf[CC[Variable[Value]]]

    /** Tries to cast the given variable to a variable over Value. */
    final def safeDowncast(x: AnyVariable): Variable[Value] = {
        checkType(x)
        unsafeDowncast(x)
    }

    /** Tries to cast the given variable collection to a collection of variables over Value. */
    final def safeDowncast[CC[X] <: TraversableOnce[X]](xs: CC[AnyVariable]): CC[Variable[Value]] = {
        xs.foreach(checkType)
        unsafeDowncast(xs)
    }

}
