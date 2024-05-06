package yuck.core

/**
 * @author Michael Marte
 *
 */
abstract class MoveEffect[V <: Value[V]] extends AnyMoveEffect {
    override def x: Variable[V]
    override def a: V
    final override def affect(space: Space) = space.setValue(x, a)
}

/**
 * @author Michael Marte
 *
 */
final class ImmutableMoveEffect
    [V <: Value[V]]
    (override val x: Variable[V], override val a: V)
    extends MoveEffect[V]
{
    override def clone = this
}

/**
 * @author Michael Marte
 *
 */
final class ReusableMoveEffectWithFixedVariable
    [V <: Value[V]]
    (override val x: Variable[V])
    extends MoveEffect[V]
{
    private var _a: V = scala.compiletime.uninitialized
    inline override def a = _a
    inline def a_=(a: V): Unit = {
        _a = a
    }
    override def clone = new ImmutableMoveEffect(x, a)
}

/**
 * @author Michael Marte
 *
 */
final class ReusableMoveEffect
    [V <: Value[V]]
    extends MoveEffect[V]
{
    private var _x: Variable[V] = scala.compiletime.uninitialized
    private var _a: V = scala.compiletime.uninitialized
    inline override def x = _x
    inline def x_=(x: Variable[V]): Unit = {
        _x = x
    }
    inline override def a = _a
    inline def a_=(a: V): Unit = {
        _a = a
    }
    inline def set(x: Variable[V], a: V): Unit = {
        _x = x
        _a = a
    }
    override def clone = new ImmutableMoveEffect(x, a)
}
