package yuck.core

/**
 * Describes a move in terms of its effects.
 *
 * @author Michael Marte
 */
abstract class Move(val id: Id[Move]) extends Ordered[Move] with Iterable[AnyVariable] {

    @inline final override def hashCode = id.rawId
    override def toString = effectsIterator.toList.sortBy(_.x).mkString(", ")
    @inline final override def compare(that: Move) = this.id.compare(that.id)

    /** Returns the effects of the move. */
    def effects: Iterable[AnyMoveEffect]

    /** Returns the effects of the move. */
    def effectsIterator: Iterator[AnyMoveEffect] = effects.iterator

    /** Returns true iff the move does not involve any variable. */
    override def isEmpty: Boolean = effects.isEmpty

    /** Returns the number of variables involved in the move. */
    override def size: Int = effects.size

    /** Iterates the variables of the move. */
    override def foreach[U](f: AnyVariable => U) = involvedVariablesIterator.foreach(f)

    /** Returns the variables involved in the move. */
    override def iterator = involvedVariablesIterator

    /** Returns the variables involved in the move. */
    def involvedVariables: Iterable[AnyVariable] = effects.view.map(_.x)

    /** Returns the variables involved in the move. */
    def involvedVariablesIterator: Iterator[AnyVariable] = effectsIterator.map(_.x)

    /** Returns true iff the given variable is involved in the move. */
    def involves(x: AnyVariable): Boolean = involvedVariablesIterator.exists(_ == x)

    /**
     * Returns the value the move would assign to the given variable.
     *
     * Throws when the given variable is not involved in the move.
     */
    def value(x: AnyVariable): AnyValue = effectsIterator.filter(_.x == x).next.a

    /**
     * Returns None if the move does not involve the given variable x,
     * otherwise it returns Some(a) where a is the value assigned to x
     * by the move.
     */
    def maybeValue(x: AnyVariable): Option[AnyValue] = effectsIterator.find(_.x == x).map(_.a)

    /**
     * Returns the value the move would assign to the given variable.
     *
     * Throws when the given variable is not involved in the move.
     */
    @inline final def value[Value <: AnyValue](x: Variable[Value]): Value =
        value(x.asInstanceOf[AnyVariable]).asInstanceOf[Value]

    /**
     * Returns None if the move does not involve the given variable x,
     * otherwise it returns Some(a) where a is the value assigned to x
     * by the move.
     */
    @inline final def maybeValue[Value <: AnyValue](x: Variable[Value]): Option[Value] =
        maybeValue(x.asInstanceOf[AnyVariable]).map(_.asInstanceOf[Value])

}
