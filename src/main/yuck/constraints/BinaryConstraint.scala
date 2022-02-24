package yuck.constraints

import yuck.core.*

/**
 * @author Michael Marte
 *
 */
abstract class BinaryConstraint
    [In <: AnyValue, Out <: AnyValue]
    (id: Id[Constraint],
     protected val x: Variable[In], protected val y: Variable[Out])
    extends Constraint(id)
{
    override def inVariables = List(x)
    override def outVariables = List(y)
    private val effect = y.reuseableEffect
    def op(x: In): Out
    override def initialize(now: SearchState) = {
        effect.a = op(now.value(x))
        effect
    }
    override def consult(before: SearchState, after: SearchState, move: Move) =
        initialize(after)
    override def commit(before: SearchState, after: SearchState, move: Move) =
        effect
}
