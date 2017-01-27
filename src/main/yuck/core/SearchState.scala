package yuck.core

import scala.collection.mutable.Cloneable

/**
 * Provides an interface for inspecting the current state of search.
 *
 * @author Michael Marte
 */
abstract class SearchState extends Cloneable[SearchState] {

    /** Returns the set of variables with value assignment. */
    def mappedVariables: Set[AnyVariable]

    override def toString =
        "{%s}".format(mappedVariables.toList.sortBy(_.name).map(x => (x -> anyValue(x)).toString).mkString(", "))

    /** Returns true iff the given variable has a value assignment. */
    def hasValue(x: AnyVariable): Boolean = maybeAnyValue(x).isDefined

    /**
     * Returns the value assigned to the given variable.
     *
     * Throws when the variable has no value assignment.
     */
    final def anyValue(x: AnyVariable): AnyValue = {
        val maybeA = maybeAnyValue(x)
        require(maybeA.isDefined, "Variable %s has no value assigned".format(x))
        maybeA.get
    }

    /**
     * Returns None if the given variable x has no value assignment;
     * otherwise returns Some(a) where a is the value assigned to x.
     */
    def maybeAnyValue(x: AnyVariable): Option[AnyValue]

    /** Typed version of anyValue. */
    final def value[Value <: AnyValue](x: Variable[Value]): Value = {
        val maybeA = maybeValue(x)
        require(maybeA.isDefined, "Variable %s has no value assigned".format(x))
        maybeA.get
    }

    /** Typed version of maybeAnyValue. */
    final def maybeValue[Value <: AnyValue](x: Variable[Value]): Option[Value] =
        maybeAnyValue(x).map(_.asInstanceOf[Value])

}
