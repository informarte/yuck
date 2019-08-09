package yuck.core

/**
 * Describes a move in terms of its effects.
 *
 * @author Michael Marte
 */
abstract class Move(val id: Id[Move]) extends Ordered[Move] with Iterable[AnyVariable] {

    @inline final override def hashCode = id.hashCode
    override def toString = effects.toList.sortBy(_.anyVariable.id).mkString(", ")
    @inline final override def compare(that: Move) = this.id.compare(that.id)

    /** Returns the effects of the move. */
    def effects: Iterable[AnyEffect]

    /** Returns true iff the move does not involve any variable. */
    override def isEmpty: Boolean = effects.isEmpty

    /** Returns the number of variables involved in the move. */
    override def size: Int = effects.size

    /** Returns an iterator over the variables involved in the move */
    override def iterator: Iterator[AnyVariable] = involvedVariables.iterator

    /** Iterates the variables of the move. */
    override def foreach[U](f: AnyVariable => U) = involvedVariables.foreach(f)

    /** Returns the variables involved in the move. */
    def involvedVariables: Iterable[AnyVariable] = effects.view.map(_.anyVariable)

    /** Returns true iff the given variable is involved in the move. */
    def involves(x: AnyVariable): Boolean = involvedVariables.exists(_ == x)

    /**
     * Returns the value the move would assign to the given variable.
     *
     * Throws when the given variable is not involved in the move.
     */
    def anyValue(x: AnyVariable): AnyValue =
        effects.iterator.filter(_.anyVariable == x).next.anyValue

    /**
     * Returns None if the move does not involve the given variable x,
     * otherwise it returns Some(a) where a is the value assigned to x
     * by the move.
     */
    def maybeAnyValue(x: AnyVariable): Option[AnyValue] =
        effects.find(_.anyVariable == x).map(_.anyValue)

    /** Typed version of anyValue. */
    @inline final def value[Value <: AnyValue](x: Variable[Value]): Value =
        anyValue(x).asInstanceOf[Value]

    /** Typed version of maybeAnyValue. */
    @inline final def maybeValue[Value <: AnyValue](x: Variable[Value]): Option[Value] =
        maybeAnyValue(x).map(_.asInstanceOf[Value])

}
