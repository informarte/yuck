package yuck.core

import scala.collection.*

/**
 * Provides an interface for inspecting the current state of search.
 *
 * @author Michael Marte
 */
abstract class SearchState extends mutable.Cloneable[SearchState] {

    /** Returns the set of variables with value assignment. */
    def mappedVariables: Set[AnyVariable]

    override def toString =
        "{%s}".format(mappedVariables.toSeq.sortBy(_.name).iterator.map(x => x -> value(x)).mkString(", "))

    /** Returns true iff the given variable has a value assignment. */
    def hasValue(x: AnyVariable): Boolean = maybeValue(x).isDefined

    /**
     * Returns the value assigned to the given variable.
     *
     * Throws when the variable has no value assignment.
     */
    def value(x: AnyVariable): AnyValue = maybeValue(x).get

    /**
     * Returns None if the given variable x has no value assignment;
     * otherwise returns Some(a) where a is the value assigned to x.
     */
    def maybeValue(x: AnyVariable): Option[AnyValue] = if (hasValue(x)) Some(value(x)) else None

    /**
     * Returns the value assigned to the given variable.
     *
     * Throws when the variable has no value assignment.
     */
    @inline final def value[V <: AnyValue](x: Variable[V]): V =
        value(x.asInstanceOf[AnyVariable]).asInstanceOf[V]

    /**
     * Returns None if the given variable x has no value assignment;
     * otherwise returns Some(a) where a is the value assigned to x.
     */
    @inline final def maybeValue[V <: AnyValue](x: Variable[V]): Option[V] =
        maybeValue(x.asInstanceOf[AnyVariable]).map(_.asInstanceOf[V])

}
