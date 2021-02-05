package yuck.constraints

import yuck.core._

/**
 * Implements binary conjunction on cost level (where 0 is true).
 *
 * @author Michael Marte
 */
final class And
    (id: Id[Constraint], override val maybeGoal: Option[Goal],
     x: BooleanVariable, y: BooleanVariable, z: BooleanVariable)
    extends TernaryConstraint(id, x, y, z)
{
    override def toString = "%s = %s /\\ %s".format(z, x, y)
    override def op(a: BooleanValue, b: BooleanValue) = BooleanValue(safeAdd(a.violation, b.violation))
    override def propagate = {
        val lhs0 = Seq(x.domain, y.domain)
        val (lhs1, dz1) = BooleanDomainPruner.conjunctionRule(lhs0, z.domain)
        val Seq(dx1, dy1) = lhs1.toSeq
        NoPropagationOccurred.pruneDomains(x, dx1, y, dy1, z, dz1)
    }
}

/**
 * Implements binary disjunction on cost level (where 0 is true).
 *
 * See [[yuck.constraints.Disjunction Disjunction]] for the cost model.
 *
 * @author Michael Marte
 *
 */
final class Or
    (id: Id[Constraint], override val maybeGoal: Option[Goal],
     x: BooleanVariable, y: BooleanVariable, z: BooleanVariable)
    extends TernaryConstraint(id, x, y, z)
{
    override def toString = "%s = %s \\/ %s".format(z, x, y)
    override def op(a: BooleanValue, b: BooleanValue) =
        if (a.truthValue || b.truthValue) True
        else BooleanValue(safeAdd(a.violation, b.violation) / 2)
    override def propagate = {
        val lhs0 = Seq(x.domain, y.domain)
        val (lhs1, dz1) = BooleanDomainPruner.disjunctionRule(lhs0, z.domain)
        val Seq(dx1, dy1) = lhs1.toSeq
        NoPropagationOccurred.pruneDomains(x, dx1, y, dy1, z, dz1)
    }
}

/**
 * Implements negation on cost level (where 0 is true).
 *
 * @author Michael Marte
 */
final class Not
    (id: Id[Constraint], override val maybeGoal: Option[Goal],
     x: BooleanVariable, y: BooleanVariable)
    extends BinaryConstraint(id, x, y)
{
    override def toString = "%s = not(%s)".format(y, x)
    override def op(a: BooleanValue) = if (a.truthValue) False else True
    override def propagate = {
        val dx0 = x.domain
        val dy0 = y.domain
        val (dx1, dy1) = BooleanDomainPruner.neRule(dx0, dy0)
        NoPropagationOccurred.pruneDomains(x, dx1, y, dy1)
    }
}
