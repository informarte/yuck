package yuck.flatzinc.compiler

import yuck.core._
import yuck.constraints.{BinaryConstraint, TernaryConstraint, ReifiedBinaryConstraintPropagator}

/**
 * @author Michael Marte
 *
 */
final class Bool2Costs1
    (id: Id[Constraint], override val maybeGoal: Option[Goal],
     x: BooleanVariable, y: IntegerVariable)
    extends BinaryConstraint(id, x, y)
{
    override def toString = "%s = bool2costs(%s)".format(y, x)
    override def op(a: BooleanValue) = IntegerValue.get(safeToInt(a.violation))
    override def propagate = {
        val (dx, dy) = Bool2CostsPropagator.bool2Costs(x.domain, y.domain)
        NoPropagationOccurred.pruneDomains(x, dx, y, dy)
    }
}

/**
 * @author Michael Marte
 *
 */
final class Bool2Costs2
    (id: Id[Constraint], override val maybeGoal: Option[Goal],
     x: BooleanVariable, y: IntegerVariable, z: BooleanVariable)
    extends TernaryConstraint(id, x, y, z)
    with ReifiedBinaryConstraintPropagator[BooleanDomain, IntegerDomain]
{
    override def toString = "bool2costs(%s, %s, %s)".format(x, y, z)
    override def op(a: BooleanValue, b: IntegerValue) =
        BooleanValue.get(abs(safeSub(a.violation, b.value)))
    override def propagate = {
        val (dx1, dy1, dz1) = propagate(x.domain, y.domain, BooleanDomain.ensureDecisionDomain(z.domain))
        NoPropagationOccurred.pruneDomains(x, dx1, y, dy1, z, dz1)
    }
    override protected def enforce(lhs: BooleanDomain, rhs: IntegerDomain) =
        Bool2CostsPropagator.bool2Costs(lhs, rhs)
    override protected def prohibit(lhs: BooleanDomain, rhs: IntegerDomain) =
        Bool2CostsPropagator.notBool2Costs(lhs, rhs)
}

/**
 * @author Michael Marte
 *
 */
object Bool2CostsPropagator {

    def bool2Costs(lhs0: BooleanDomain, rhs0: IntegerDomain) = {
        val lhs1 = BooleanDomain.ensureDecisionDomain(lhs0)
        val lhs2 =
            BooleanDecisionDomain.createDomain(
                lhs1.contains(False) && (! rhs0.hasUb || rhs0.ub > Zero),
                lhs1.contains(True)  && (! rhs0.hasLb || rhs0.lb <= Zero))
        val rhs2 =
            rhs0.intersect(
                IntegerDomain.createRange(
                    if (lhs1.contains(True)) Zero else One,
                    if (lhs1.contains(False)) null else Zero))
        (lhs2, rhs2)
    }

    def notBool2Costs(lhs0: BooleanDomain, rhs0: IntegerDomain) = {
        (lhs0, rhs0)
    }

}
