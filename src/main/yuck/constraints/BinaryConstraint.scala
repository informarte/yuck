package yuck.constraints

import yuck.core.*

abstract class BinaryConstraint
    [A <: Value[A], D <: Domain[A, D], X <: Variable[A, D, X],
     B <: Value[B], E <: Domain[B, E], Y <: Variable[B, E, Y]]
    (id: Id[Constraint],
     protected val x: X,
     protected val y: Y)
    extends Constraint(id)
{
    override def inVariables = List(x)
    override def outVariables = List(y)
    private val effect = y.reuseableEffect
    def op(a: A): B
    override def initialize(now: SearchState) = {
        effect.a = op(now.value(x))
        effect
    }
    override def consult(before: SearchState, after: SearchState, move: Move) =
        initialize(after)
    override def commit(before: SearchState, after: SearchState, move: Move) =
        effect
}
