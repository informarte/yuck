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
    [Value <: AnyValue]
    (id: Id[Move],
     override val effects: Iterable[MoveEffect[Value]])
    extends Move(id)

/**
 * Turns the given (variable, value) pair into a move.
 *
 * @author Michael Marte
 */
final class ChangeValue
    [Value <: AnyValue]
    (id: Id[Move],
     x: Variable[Value], a: Value)
    extends ChangeAnyValues(id, new ImmutableMoveEffect(x, a))
