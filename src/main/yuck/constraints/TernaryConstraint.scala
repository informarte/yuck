package yuck.constraints

import yuck.core.*

abstract class TernaryConstraint
    [A <: Value[A], D <: Domain[A, D], X <: Variable[A, D, X],
     B <: Value[B], E <: Domain[B, E], Y <: Variable[B, E, Y],
     C <: Value[C], F <: Domain[C, F], Z <: Variable[C, F, Z]]
    (id: Id[Constraint],
     protected val x: X,
     protected val y: Y,
     protected val z: Z)
    extends Constraint(id)
{
    final override def inVariables = List(x, y)
    final override def outVariables = List(z)
    protected def op(a: A, b: B): C
    private val effect = z.reuseableEffect
    final override def initialize(now: SearchState) = {
        effect.a = op(now.value(x), now.value(y))
        effect
    }
    final override def consult(before: SearchState, after: SearchState, move: Move) =
        initialize(after)
    final override def commit(before: SearchState, after: SearchState, move: Move) =
        effect
}

trait ReifiedBinaryConstraintPropagator[LhsDomain <: AnyDomain, RhsDomain <: AnyDomain] {
    protected def enforce(lhs: LhsDomain, rhs: RhsDomain): (LhsDomain, RhsDomain)
    protected def prohibit(lhs: LhsDomain, rhs: RhsDomain): (LhsDomain, RhsDomain)
    final def propagate
        (dx0: LhsDomain, dy0: RhsDomain, dz0: BooleanDomain):
        (LhsDomain, RhsDomain, BooleanDomain) =
    {
        if dz0 == TrueDomain then {
            val (dx1, dy1) = enforce(dx0, dy0)
            (dx1, dy1, dz0)
        } else if dz0 == FalseDomain then {
            val (dx1, dy1) = prohibit(dx0, dy0)
            (dx1, dy1, dz0)
        } else {
            val (dx2, dy2) = enforce(dx0, dy0)
            if dx2.isEmpty || dy2.isEmpty then {
                (dx0, dy0, FalseDomain)
            } else {
                val (dx3, dy3) = prohibit(dx0, dy0)
                if dx3.isEmpty || dy3.isEmpty then {
                    (dx0, dy0, TrueDomain)
                } else {
                    (dx0, dy0, dz0)
                }
            }
        }
    }
}
