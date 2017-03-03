package yuck.core

import scala.language.higherKinds

/**
 * Provides an interface for working with values of unknown type.
 *
 * @author Michael Marte
 */
abstract class AnyValue {

    /** Returns the type of the concrete value. */
    @inline final def valueType: Class[_ <: AnyValue] = getClass

}

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
trait AnyValueTraits[Value <: AnyValue] {

    /** Returns the type of the concrete values. */
    val valueType: Class[Value]

    /** Returns the largest domain over Value. */
    val unboundedDomain: Domain[Value]

    /** Throws when the type of the given value differs from Value. */
    final def checkType(a: AnyValue) {
        assert(
            a.valueType == valueType,
            "Value %s has type %s but not %s as expected".format(a, a.valueType, valueType))
    }

    /** Casts the given value to Value. */
    private final def staticDowncast(a: AnyValue): Value =
        a.asInstanceOf[Value]

    /** Tries to cast the given value to Value. */
    final def dynamicDowncast(a: AnyValue): Value = {
        checkType(a)
        staticDowncast(a)
    }

    /** Throws when the given domain is not a domain over Value. */
    final def checkType(d: AnyDomain) {
        assert(
            d.valueType == valueType,
            "Domain %s has type %s but not %s as expected".format(d, d.valueType, valueType))
    }

    /** Casts the given domain to a domain over Value. */
    private final def staticDowncast(x: AnyDomain): Domain[Value] =
        x.asInstanceOf[Domain[Value]]

    /** Tries to cast the given domain to a domain over Value. */
    final def dynamicDowncast(d: AnyDomain): Domain[Value] = {
        checkType(d)
        staticDowncast(d)
    }

    /** Throws when the given variable is not a variable over Value. */
    final def checkType(x: AnyVariable) {
        assert(
            x.valueType == valueType,
            "Variable %s has type %s but not %s as expected".format(x.name, x.valueType, valueType))
    }

    /** Casts the given variable to a variable over Value. */
    final def staticDowncast(x: AnyVariable): Variable[Value] =
        x.asInstanceOf[Variable[Value]]

    /** Casts the given variable collection to a collection of variables over Value. */
    final def staticDowncast[CC[X] <: TraversableOnce[X]](xs: CC[AnyVariable]): CC[Variable[Value]] =
        xs.asInstanceOf[CC[Variable[Value]]]

    /** Tries to cast the given variable to a variable over Value. */
    final def dynamicDowncast(x: AnyVariable): Variable[Value] = {
        checkType(x)
        staticDowncast(x)
    }

    /** Tries to cast the given variable collection to a collection of variables over Value. */
    final def dynamicDowncast[CC[X] <: TraversableOnce[X]](xs: CC[AnyVariable]): CC[Variable[Value]] = {
        xs.foreach(checkType)
        staticDowncast(xs)
    }

}
