package yuck.constraints

import yuck.core.*

/**
 * @author Michael Marte
 *
 */
abstract class TernaryConstraint
    [In1 <: Value[In1], In2 <: Value[In2], Out <: Value[Out]]
    (id: Id[Constraint],
     protected val x: Variable[In1], protected val y: Variable[In2], protected val z: Variable[Out])
    extends Constraint(id)
{
    final override def inVariables = List(x, y)
    final override def outVariables = List(z)
    protected def op(a: In1, b: In2): Out
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

/**
 * @author Michael Marte
 *
 */
trait ReifiedBinaryConstraintPropagator[LhsDomain <: AnyDomain, RhsDomain <: AnyDomain] {
    protected def enforce(lhs: LhsDomain, rhs: RhsDomain): (LhsDomain, RhsDomain)
    protected def prohibit(lhs: LhsDomain, rhs: RhsDomain): (LhsDomain, RhsDomain)
    final def propagate
        (dx0: LhsDomain, dy0: RhsDomain, dz0: BooleanDomain):
        (LhsDomain, RhsDomain, BooleanDomain) =
    {
        if (dz0 == TrueDomain) {
            val (dx1, dy1) = enforce(dx0, dy0)
            (dx1, dy1, dz0)
        } else if (dz0 == FalseDomain) {
            val (dx1, dy1) = prohibit(dx0, dy0)
            (dx1, dy1, dz0)
        } else {
            val (dx2, dy2) = enforce(dx0, dy0)
            if (dx2.isEmpty || dy2.isEmpty) {
                (dx0, dy0, FalseDomain)
            } else {
                val (dx3, dy3) = prohibit(dx0, dy0)
                if (dx3.isEmpty || dy3.isEmpty) {
                    (dx0, dy0, TrueDomain)
                } else {
                    (dx0, dy0, dz0)
                }
            }
        }
    }
}
