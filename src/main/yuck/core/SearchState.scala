package yuck.core

import scala.collection._

/**
 * Provides an interface for inspecting the current state of search.
 *
 * @author Michael Marte
 */
abstract class SearchState extends mutable.Cloneable[SearchState] {

    /** Returns the set of variables with value assignment. */
    def mappedVariables: Set[AnyVariable]

    override def toString =
        "{%s}".format(mappedVariables.toList.sortBy(_.name).iterator.map(x => (x -> anyValue(x)).toString).mkString(", "))

    /** Returns true iff the given variable has a value assignment. */
    def hasValue(x: AnyVariable): Boolean = maybeAnyValue(x).isDefined

    /**
     * Returns the value assigned to the given variable.
     *
     * Throws when the variable has no value assignment.
     */
    def anyValue(x: AnyVariable): AnyValue = maybeAnyValue(x).get

    /**
     * Returns None if the given variable x has no value assignment;
     * otherwise returns Some(a) where a is the value assigned to x.
     */
    def maybeAnyValue(x: AnyVariable): Option[AnyValue] =
        if (hasValue(x)) Some(anyValue(x)) else None

    /** Typed version of anyValue. */
    @inline final def value[Value <: AnyValue](x: Variable[Value]): Value =
        anyValue(x).asInstanceOf[Value]

    /** Typed version of maybeAnyValue. */
    @inline final def maybeValue[Value <: AnyValue](x: Variable[Value]): Option[Value] =
        maybeAnyValue(x).map(_.asInstanceOf[Value])

}
