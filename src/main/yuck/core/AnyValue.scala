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
 * Instead use value traits for type checking and dynamic up-casting!
 *
 * @author Michael Marte
 */
trait AnyValueTraits[Value <: AnyValue] {

    /** Returns the type of the concrete values. */
    val valueType: Class[Value]

    /** Returns the largest domain over Value. */
    val unboundedDomain: Domain[Value]

    /** Decides whether lhs is a subset of rhs. */
    def isSubsetOf(lhs: Domain[Value], rhs: Domain[Value]): Boolean

    /** Throws when the type of the given value differs from Value. */
    final def checkType(a: AnyValue) {
        assert(
            a.valueType == valueType,
            "Value %s has type %s but not %s as expected".format(a, a.valueType, valueType))
    }

    /** Casts the given value to Value. */
    final def staticCast(a: AnyValue): Value =
        a.asInstanceOf[Value]

    /** Tries to cast the given value to Value. */
    final def dynamicCast(a: AnyValue): Value = {
        checkType(a)
        staticCast(a)
    }

    /** Throws when the given domain is not a domain over Value. */
    final def checkType(d: AnyDomain) {
        assert(
            d.valueType == valueType,
            "Domain %s has type %s but not %s as expected".format(d, d.valueType, valueType))
    }

    /** Casts the given domain to a domain over Value. */
    final def staticCast(x: AnyDomain): Domain[Value] =
        x.asInstanceOf[Domain[Value]]

    /** Tries to cast the given domain to a domain over Value. */
    final def dynamicCast(d: AnyDomain): Domain[Value] = {
        checkType(d)
        staticCast(d)
    }

    /** Throws when the given variable is not a variable over Value. */
    final def checkType(x: AnyVariable) {
        assert(
            x.valueType == valueType,
            "Variable %s has type %s but not %s as expected".format(x.name, x.valueType, valueType))
    }

    /** Casts the given variable to a variable over Value. */
    final def staticCast(x: AnyVariable): Variable[Value] =
        x.asInstanceOf[Variable[Value]]

    /** Casts the given variable collection to a collection of variables over Value. */
    final def staticCast[CC[X] <: TraversableOnce[X]](xs: CC[AnyVariable]): CC[Variable[Value]] =
        xs.asInstanceOf[CC[Variable[Value]]]

    /** Tries to cast the given variable to a variable over Value. */
    final def dynamicCast(x: AnyVariable): Variable[Value] = {
        checkType(x)
        staticCast(x)
    }

    /** Tries to cast the given variable collection to a collection of variables over Value. */
    final def dynamicCast[CC[X] <: TraversableOnce[X]](xs: CC[AnyVariable]): CC[Variable[Value]] = {
        xs.foreach(x => checkType(x))
        staticCast(xs)
    }

}
