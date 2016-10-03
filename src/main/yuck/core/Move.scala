package yuck.core

/**
 * Describes a move in terms of its effects.
 *
 * @author Michael Marte
 */
abstract class Move(val id: Id[Move]) extends Ordered[Move] {

    @inline final override def hashCode = id.hashCode
    override def toString = effects.toList.sortBy(_.anyVariable.id).toString
    @inline final override def compare(that: Move) = this.id.compare(that.id)

    /** Returns the effects of the move. */
    def effects: TraversableOnce[AnyEffect]

    /** Returns true iff the move does not involve any variable. */
    def isEmpty: Boolean = effects.isEmpty

    /** Returns the number of variables involved in the move. */
    def size: Int = effects.size

    /** Returns the variables involved in the move. */
    def involvedVariables: TraversableOnce[AnyVariable] = effects.toIterator.map(_.anyVariable)

    /** Returns true iff the given variable is involved in the move. */
    def involves(x: AnyVariable): Boolean = involvedVariables.exists(_ == x)

    /**
     * Returns the value the move would assign to the given variable.
     *
     * Throws when the given variable is not involved in the move.
     */
    @inline final def anyValue(x: AnyVariable): AnyValue = maybeAnyValue(x).get

    /**
     * Returns None if the move does not involve the given variable x,
     * otherwise it returns Some(a) where a is the value assigned to x
     * by the move.
     */
    def maybeAnyValue(x: AnyVariable): Option[AnyValue] =
        effects.find(_.anyVariable.id == x.id).map(_.anyValue)

    /** Typed version of anyValue. */
    @inline final def value[Value <: AnyValue](x: Variable[Value]): Value = maybeValue(x).get

    /** Typed version of maybeAnyValue. */
    @inline final def maybeValue[Value <: AnyValue](x: Variable[Value]): Option[Value] =
        maybeAnyValue(x).map(_.asInstanceOf[Value])

}
