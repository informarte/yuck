package yuck.flatzinc.compiler

import yuck.core._
import yuck.constraints.{BinaryConstraint, TernaryConstraint, ReifiedBinaryConstraintPropagator}

/**
 * @author Michael Marte
 *
 */
final class Bool2Int1
    (id: Id[Constraint], override val maybeGoal: Option[Goal],
     x: BooleanVariable, y: IntegerVariable)
    extends BinaryConstraint(id, x, y)
{
    override def toString = "%s = bool2int(%s)".format(y, x)
    override def op(a: BooleanValue) = if (a.truthValue) One else Zero
    override def propagate = {
        val (dx, dy) = Bool2IntPropagator.bool2Int(x.domain, y.domain)
        NoPropagationOccurred.pruneDomains(x, dx, y, dy)
    }
}

/**
 * @author Michael Marte
 *
 */
final class Bool2Int2
    (id: Id[Constraint], override val maybeGoal: Option[Goal],
     x: BooleanVariable, y: IntegerVariable, z: BooleanVariable)
    extends TernaryConstraint(id, x, y, z)
    with ReifiedBinaryConstraintPropagator[BooleanDomain, IntegerDomain]
{
    override def toString = "bool2int(%s, %s, %s)".format(x, y, z)
    override def op(a: BooleanValue, b: IntegerValue) =
        if ((a.truthValue && b == One) || (! a.truthValue && b == Zero)) True else False
    override def propagate = {
        val (dx1, dy1, dz1) = propagate(x.domain, y.domain, BooleanDomain.ensureDecisionDomain(z.domain))
        NoPropagationOccurred.pruneDomains(x, dx1, y, dy1, z, dz1)
    }
    override protected def enforce(lhs: BooleanDomain, rhs: IntegerDomain) =
        Bool2IntPropagator.bool2Int(lhs, rhs)
    override protected def prohibit(lhs: BooleanDomain, rhs: IntegerDomain) =
        Bool2IntPropagator.notBool2Int(lhs, rhs)
}

/**
 * @author Michael Marte
 *
 */
object Bool2IntPropagator {

    def bool2Int(lhs0: BooleanDomain, rhs0: IntegerDomain) = {
        val lhs1 = BooleanDomain.ensureDecisionDomain(lhs0)
        val lhs2 = BooleanDecisionDomain(rhs0.contains(Zero) && lhs1.contains(False), rhs0.contains(One) && lhs1.contains(True))
        val rhs2 = rhs0.intersect(IntegerRange(if (lhs1.contains(False)) Zero else One, if (lhs1.contains(True)) One else Zero))
        (lhs2, rhs2)
    }

    def notBool2Int(lhs0: BooleanDomain, rhs0: IntegerDomain) = {
        val lhs1 = BooleanDomain.ensureDecisionDomain(lhs0)
        val lhs2 = BooleanDecisionDomain(rhs0.contains(One) && lhs1.contains(False), rhs0.contains(Zero) && lhs1.contains(True))
        val rhs2 = rhs0.intersect(IntegerRange(if (lhs1.contains(True)) Zero else One, if (lhs1.contains(False)) One else Zero))
        (lhs2, rhs2)
    }

}
