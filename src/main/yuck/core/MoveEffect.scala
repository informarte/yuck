package yuck.core

abstract class MoveEffect[A <: Value[A], D <: Domain[A, D], X <: Variable[A, D, X]] extends AnyMoveEffect {
    override def x: X
    override def a: A
    final override def affect(space: Space) = space.setValue(x, a)
}

final class ImmutableMoveEffect
    [A <: Value[A], D <: Domain[A, D], X <: Variable[A, D, X]]
    (override val x: X, override val a: A)
    extends MoveEffect[A, D, X]
{
    override def clone = this
}

final class ReusableMoveEffectWithFixedVariable
    [A <: Value[A], D <: Domain[A, D], X <: Variable[A, D, X]]
    (override val x: X)
    extends MoveEffect[A, D, X]
{
    private var _a: A = scala.compiletime.uninitialized
    inline override def a = _a
    inline def a_=(a: A): Unit = {
        _a = a
    }
    override def clone = new ImmutableMoveEffect(x, a)
}

final class ReusableMoveEffect
    [A <: Value[A], D <: Domain[A, D], X <: Variable[A, D, X]]
    extends MoveEffect[A, D, X]
{
    private var _x: X = scala.compiletime.uninitialized
    private var _a: A = scala.compiletime.uninitialized
    inline override def x = _x
    inline def x_=(x: X): Unit = {
        _x = x
    }
    inline override def a = _a
    inline def a_=(a: A): Unit = {
        _a = a
    }
    inline def set(x: X, a: A): Unit = {
        _x = x
        _a = a
    }
    override def clone = new ImmutableMoveEffect(x, a)
}
