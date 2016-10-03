package yuck.constraints

import yuck.core._

/**
 * @author Michael Marte
 *
 */
abstract class UnaryConstraint
    [In <: AnyValue, Out <: AnyValue]
    (id: Id[Constraint], goal: Goal,
     a: Variable[In], b: Variable[Out])
    extends Constraint(id, goal)
{
    override def inVariables = List(a)
    override def outVariables = List(b)
    private val effects = List(new ReusableEffectWithFixedVariable[Out](b))
    private val effect = effects.head
    def op(x: In): Out
    override def initialize(now: SearchState) = {
        effect.a = op(now.value(a))
        effects
    }
    override def consult(before: SearchState, after: SearchState, move: Move) =
        initialize(after)
    override def commit(before: SearchState, after: SearchState, move: Move) =
        effects
}
