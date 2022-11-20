package yuck.core

/**
 * Turns the given effects into a move.
 *
 * @author Michael Marte
 */
final class ChangeAnyValues
    (id: Id[Move],
     override val effects: Iterable[AnyMoveEffect])
    extends Move(id)

/**
 * Turns the given effects into a move.
 *
 * @author Michael Marte
 */
final class ChangeValues
    [V <: Value[V]]
    (id: Id[Move],
     override val effects: Iterable[MoveEffect[V]])
    extends Move(id)

/**
 * Turns the given (variable, value) pair into a move.
 *
 * @author Michael Marte
 */
final class ChangeValue
    [V <: Value[V]]
    (id: Id[Move],
     x: Variable[V], a: V)
    extends Move(id)
{
    override val effects = List(new ImmutableMoveEffect(x, a))
}
