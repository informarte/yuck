package yuck.core

/**
 * @author Michael Marte
 *
 */
abstract class MoveEffect[Value <: AnyValue] extends AnyMoveEffect

/**
 * @author Michael Marte
 *
 */
final class ImmutableMoveEffect
    [Value <: AnyValue]
    (val x: Variable[Value], val a: Value)
    extends MoveEffect[Value]
{
    override def anyVariable = x
    override def anyValue = a
    override def affect(space: Space) = space.setValue(x, a)
    override def clone = this
}

/**
 * @author Michael Marte
 *
 */
final class ReusableMoveEffectWithFixedVariable
    [Value <: AnyValue]
    (val x: Variable[Value])
    extends MoveEffect[Value]
{
    var a: Value = _
    override def anyVariable = x
    override def anyValue = a
    override def affect(space: Space) = space.setValue(x, a)
    override def clone = new ImmutableMoveEffect(x, a)
    def setValue(a: Value): Unit = {
        this.a = a
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
    var x: Variable[Value] = _
    var a: Value = _
    override def anyVariable = x
    override def anyValue = a
    override def affect(space: Space) = space.setValue(x, a)
    override def clone = new ImmutableMoveEffect(x, a)
    @inline def set(x: Variable[Value], a: Value): Unit = {
        this.x = x
        this.a = a
    }
}
