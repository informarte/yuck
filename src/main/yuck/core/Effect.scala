package yuck.core

/**
 * @author Michael Marte
 *
 */
abstract class Effect[Value <: AnyValue] extends AnyEffect {
}

/**
 * @author Michael Marte
 *
 */
final class ImmutableEffect
    [Value <: AnyValue]
    (val x: Variable[Value], val a: Value)
    extends Effect[Value]
{
    override def anyVariable = x
    override def anyValue = a
    override def setValue(assignment: Assignment) = assignment.setValue(x, a)
    override def clone = this
}

/**
 * @author Michael Marte
 *
 */
final class ReusableEffectWithFixedVariable
    [Value <: AnyValue]
    (val x: Variable[Value])
    extends Effect[Value]
{
    var a: Value = _
    override def anyVariable = x
    override def anyValue = a
    override def setValue(assignment: Assignment) = assignment.setValue(x, a)
    override def clone = new ImmutableEffect(x, a)
    def setNextRandomValue(searchState: SearchState, randomGenerator: RandomGenerator) {
        a = x.domain.nextRandomValue(randomGenerator, searchState.value(x))
    }
}

/**
 * @author Michael Marte
 *
 */
final class ReusableEffect
    [Value <: AnyValue]
    extends Effect[Value]
{
    var x: Variable[Value] = _
    var a: Value = _
    override def anyVariable = x
    override def anyValue = a
    override def setValue(assignment: Assignment) = assignment.setValue(x, a)
    override def clone = new ImmutableEffect(x, a)
    @inline def set(x: Variable[Value], a: Value) {
        this.x = x
        this.a = a
    }
}
