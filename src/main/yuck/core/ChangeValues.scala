package yuck.core

/**
 * Turns the given effects into a move.
 *
 * @author Michael Marte
 */
class ChangeAnyValues
    (id: Id[Move],
     override val effects: Iterable[AnyMoveEffect])
    extends Move(id)
{
    override def effectsIterator = effects.iterator
    override lazy val size = effects.size
}

/**
 * Turns the given effects into a move.
 *
 * @author Michael Marte
 */
class ChangeValues
    [Value <: AnyValue]
    (id: Id[Move],
     override val effects: Iterable[MoveEffect[Value]])
    extends Move(id)
{
    override def effectsIterator = effects.iterator
    override lazy val size = effects.size
}

/**
 * Turns the given (variable, value) pair into a move.
 *
 * @author Michael Marte
 */
final class ChangeValue
    [Value <: AnyValue]
    (id: Id[Move],
     val x: Variable[Value], val a: Value)
    extends ChangeValues[Value](id, List(new ImmutableMoveEffect(x, a)))
{}
