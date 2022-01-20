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

/**
 * Turns the given effects into a move.
 *
 * @author Michael Marte
 */
class ChangeValues
    [V <: AnyValue]
    (id: Id[Move],
     override val effects: Iterable[MoveEffect[V]])
    extends Move(id)

/**
 * Turns the given (variable, value) pair into a move.
 *
 * @author Michael Marte
 */
final class ChangeValue
    [V <: AnyValue]
    (id: Id[Move],
     x: Variable[V], a: V)
    extends ChangeAnyValues(id, new ImmutableMoveEffect(x, a))
