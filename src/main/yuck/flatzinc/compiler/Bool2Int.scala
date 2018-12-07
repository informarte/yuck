package yuck.flatzinc.compiler

import yuck.core._
import yuck.constraints.{BinaryConstraint, TernaryConstraint, ReifiedBinaryConstraintPropagator}

/**
 * @author Michael Marte
 *
 */
final class Bool2Int1
    (id: Id[Constraint], goal: Goal,
     x: Variable[BooleanValue], y: Variable[IntegerValue])
    extends BinaryConstraint(id, goal, x, y)
{
    override def toString = "bool2int(%s, %s)".format(x, y)
    override def op(a: BooleanValue) = if (a.truthValue) One else Zero
    override def propagate = {
        val (dx, dy) = Bool2IntPropagator.bool2Int(x.domain, y.domain)
        Variable.pruneDomains(x, dx, y, dy)
    }
}

/**
 * @author Michael Marte
 *
 */
final class Bool2Int2
    (id: Id[Constraint], goal: Goal,
     x: Variable[BooleanValue], y: Variable[IntegerValue], z: Variable[BooleanValue])
    extends TernaryConstraint(id, goal, x, y, z)
    with ReifiedBinaryConstraintPropagator[BooleanValue, IntegerValue]
{
    override def toString = "bool2int(%s, %s, %s)".format(x, y, z)
    override def op(a: BooleanValue, b: IntegerValue) =
        if ((a.truthValue && b == One) || (! a.truthValue && b == Zero)) True else False
    override def propagate = propagate(x, y, z)
    override protected def enforce(lhs: Domain[BooleanValue], rhs: Domain[IntegerValue]) =
        Bool2IntPropagator.bool2Int(lhs, rhs)
    override protected def prohibit(lhs: Domain[BooleanValue], rhs: Domain[IntegerValue]) =
        Bool2IntPropagator.notBool2Int(lhs, rhs)
}

/**
 * @author Michael Marte
 *
 */
final object Bool2IntPropagator {

    def bool2Int(lhs0: Domain[BooleanValue], rhs0: Domain[IntegerValue]) = {
        val lhs1 = BooleanDomain.ensureDecisionDomain(lhs0)
        val lhs2 = BooleanDecisionDomain.createDomain(rhs0.contains(Zero) && lhs1.contains(False), rhs0.contains(One) && lhs1.contains(True))
        val rhs2 = rhs0.intersect(IntegerDomain.createRange(if (lhs1.contains(False)) Zero else One, if (lhs1.contains(True)) One else Zero))
        (lhs2, rhs2)
    }

    def notBool2Int(lhs0: Domain[BooleanValue], rhs0: Domain[IntegerValue]) = {
        val lhs1 = BooleanDomain.ensureDecisionDomain(lhs0)
        val lhs2 = BooleanDecisionDomain.createDomain(rhs0.contains(One) && lhs1.contains(False), rhs0.contains(Zero) && lhs1.contains(True))
        val rhs2 = rhs0.intersect(IntegerDomain.createRange(if (lhs1.contains(True)) Zero else One, if (lhs1.contains(False)) One else Zero))
        (lhs2, rhs2)
    }

}
