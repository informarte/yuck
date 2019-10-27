package yuck.constraints

import yuck.core._

/**
 * @author Michael Marte
 *
 */
abstract class BinaryConstraint
    [In <: AnyValue, Out <: AnyValue]
    (id: Id[Constraint],
     x: Variable[In], y: Variable[Out])
    extends Constraint(id)
{
    override def inVariables = List(x)
    override def outVariables = List(y)
    private val effects = List(new ReusableMoveEffectWithFixedVariable[Out](y))
    private val effect = effects.head
    def op(x: In): Out
    override def initialize(now: SearchState) = {
        effect.a = op(now.value(x))
        effects
    }
    override def consult(before: SearchState, after: SearchState, move: Move) =
        initialize(after)
    override def commit(before: SearchState, after: SearchState, move: Move) =
        effects
}
