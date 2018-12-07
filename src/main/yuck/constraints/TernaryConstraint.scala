package yuck.constraints

import yuck.core._

/**
 * @author Michael Marte
 *
 */
abstract class TernaryConstraint
    [In1 <: AnyValue, In2 <: AnyValue, Out <: AnyValue]
    (id: Id[Constraint], goal: Goal,
     x: Variable[In1], y: Variable[In2], z: Variable[Out])
    extends Constraint(id, goal)
{
    final override def inVariables = List(x, y)
    final override def outVariables = List(z)
    protected def op(x: In1, y: In2): Out
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

/**
 * @author Michael Marte
 *
 */
trait ReifiedBinaryConstraintPropagator[LhsValue <: AnyValue, RhsValue <: AnyValue] {
    protected def enforce(lhs: Domain[LhsValue], rhs: Domain[RhsValue]): (Domain[LhsValue], Domain[RhsValue])
    protected def prohibit(lhs: Domain[LhsValue], rhs: Domain[RhsValue]): (Domain[LhsValue], Domain[RhsValue])
    final def propagate(x: Variable[LhsValue], y: Variable[RhsValue], z: Variable[BooleanValue]): Boolean = {
        val dx0 = x.domain
        val dy0 = y.domain
        val dz0 = BooleanDomain.ensureDecisionDomain(z.domain)
        val (dx1, dy1, dz1) =
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
        Variable.pruneDomains(x, dx1, y, dy1, z, dz1)
    }
}

