package yuck.constraints

import yuck.core._

/**
 * @author Michael Marte
 *
 */
abstract class BinaryConstraint
    [In1 <: AnyValue, In2 <: AnyValue, Out <: AnyValue]
    (id: Id[Constraint], goal: Goal,
     x: Variable[In1], y: Variable[In2], z: Variable[Out])
    extends Constraint(id, goal)
{
    final override def inVariables = List(x, y)
    final override def outVariables = List(z)
    def op(x: In1, y: In2): Out
    private val effects = List(new ReusableEffectWithFixedVariable[Out](z))
    private val effect = effects.head
    final override def initialize(now: SearchState) = {
        effect.a = op(now.value(x), now.value(y))
        effects
    }
    final override def consult(before: SearchState, after: SearchState, move: Move) =
        initialize(after)
    final override def commit(before: SearchState, after: SearchState, move: Move) =
        effects
}
