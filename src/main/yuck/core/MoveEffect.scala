package yuck.core

/**
 * @author Michael Marte
 *
 */
abstract class MoveEffect[Value <: AnyValue] extends AnyMoveEffect {
    override def x: Variable[Value]
    override def a: Value
    final override def affect(space: Space) = space.setValue(x, a)
}

/**
 * @author Michael Marte
 *
 */
final class ImmutableMoveEffect
    [Value <: AnyValue]
    (override val x: Variable[Value], override val a: Value)
    extends MoveEffect[Value]
{
    override def clone = this
}

/**
 * @author Michael Marte
 *
 */
final class ReusableMoveEffectWithFixedVariable
    [Value <: AnyValue]
    (override val x: Variable[Value])
    extends MoveEffect[Value]
{
    private var _a: Value = _
    override def a = _a
    def a_=(a: Value): Unit = {
        _a = a
    }
    override def clone = new ImmutableMoveEffect(x, a)
    def setValue(a: Value): Unit = {
        _a = a
    }
}

/**
 * @author Michael Marte
 *
 */
final class ReusableMoveEffect
    [Value <: AnyValue]
    extends MoveEffect[Value]
{
    private var _x: Variable[Value] = _
    private var _a: Value = _
    override def x = _x
    def x_=(x: Variable[Value]): Unit = {
        _x = x
    }
    override def a = _a
    def a_=(a: Value): Unit = {
        _a = a
    }
    override def clone = new ImmutableMoveEffect(x, a)
    @inline def set(x: Variable[Value], a: Value): Unit = {
        _x = x
        _a = a
    }
}
