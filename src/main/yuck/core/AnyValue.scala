package yuck.core

/**
 * Provides an interface for working with values of unknown type.
 *
 * @author Michael Marte
 */
abstract class AnyValue {

    /** Returns the type of the concrete value. */
    @inline final def valueType: Class[_ <: AnyValue] = getClass

}
