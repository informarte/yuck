package yuck.core

/**
 * Turns the given effects into a move.
 */
final class ChangeAnyValues
    (id: Id[Move],
     override val effects: Iterable[AnyMoveEffect])
    extends Move(id)

/**
 * Turns the given effects into a move.
 */
final class ChangeValues
    [A <: Value[A], D <: Domain[A, D], X <: Variable[A, D, X]]
    (id: Id[Move],
     override val effects: Iterable[MoveEffect[A, D, X]])
    extends Move(id)

/**
 * Turns the given (variable, value) pair into a move.
 */
final class ChangeValue
    [A <: Value[A], D <: Domain[A, D], X <: Variable[A, D, X]]
    (id: Id[Move], x: X, a: A)
    extends Move(id)
{
    override val effects = List(new ImmutableMoveEffect(x, a))
}
