package yuck.core

/**
 * Provides an interface for working with values of unknown type.
 */
abstract class AnyValue {

    /** Returns the type of the concrete value. */
    inline final def valueType: Class[? <: AnyValue] = getClass

}
