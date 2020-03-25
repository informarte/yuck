package yuck.constraints

import yuck.core._

/**
 * @author Michael Marte
 *
 */
final class Eq
    [Value <: OrderedValue[Value]]
    (id: Id[Constraint], override val maybeGoal: Option[Goal],
     x: OrderedVariable[Value], y: OrderedVariable[Value], z: BooleanVariable)
    (implicit valueTraits: OrderedValueTraits[Value])
    extends TernaryConstraint(id, x, y, z)
    with ReifiedBinaryConstraintPropagator[OrderedDomain[Value], OrderedDomain[Value]]
{
    override def toString = "eq(%s, %s, %s)".format(x, y, z)
    override def op(a: Value, b: Value) = valueTraits.orderingCostModel.eqViolation(a, b)
    override def propagate = {
        val (dx1, dy1, dz1) = propagate(x.domain, y.domain, BooleanDomain.ensureDecisionDomain(z.domain))
        NoPropagationOccurred.pruneDomains(x, dx1, y, dy1, z, dz1)
    }
    override protected def enforce(lhs: OrderedDomain[Value], rhs: OrderedDomain[Value]) =
        valueTraits.domainPruner.eqRule(lhs, rhs)
    override protected def prohibit(lhs: OrderedDomain[Value], rhs: OrderedDomain[Value]) =
        valueTraits.domainPruner.neRule(lhs, rhs)
}

/**
 * @author Michael Marte
 *
 */
final class Ne
    [Value <: OrderedValue[Value]]
    (id: Id[Constraint], override val maybeGoal: Option[Goal],
     x: OrderedVariable[Value], y: OrderedVariable[Value], z: BooleanVariable)
    (implicit valueTraits: OrderedValueTraits[Value])
    extends TernaryConstraint(id, x, y, z)
    with ReifiedBinaryConstraintPropagator[OrderedDomain[Value], OrderedDomain[Value]]
{
    override def toString = "ne(%s, %s, %s)".format(x, y, z)
    override def op(a: Value, b: Value) = valueTraits.orderingCostModel.neViolation(a, b)
    override def propagate = {
        val (dx1, dy1, dz1) = propagate(x.domain, y.domain, BooleanDomain.ensureDecisionDomain(z.domain))
        NoPropagationOccurred.pruneDomains(x, dx1, y, dy1, z, dz1)
    }
    override protected def enforce(lhs: OrderedDomain[Value], rhs: OrderedDomain[Value]) =
        valueTraits.domainPruner.neRule(lhs, rhs)
    override protected def prohibit(lhs: OrderedDomain[Value], rhs: OrderedDomain[Value]) =
        valueTraits.domainPruner.eqRule(lhs, rhs)
}

/**
 * @author Michael Marte
 *
 */
final class Lt
    [Value <: OrderedValue[Value]]
    (id: Id[Constraint], override val maybeGoal: Option[Goal],
     x: OrderedVariable[Value], y: OrderedVariable[Value], z: BooleanVariable)
    (implicit valueTraits: OrderedValueTraits[Value])
    extends TernaryConstraint(id, x, y, z)
    with ReifiedBinaryConstraintPropagator[OrderedDomain[Value], OrderedDomain[Value]]
{
    override def toString = "lt(%s, %s, %s)".format(x, y, z)
    override def op(a: Value, b: Value) = valueTraits.orderingCostModel.ltViolation(a, b)
    override def propagate = {
        val (dx1, dy1, dz1) = propagate(x.domain, y.domain, BooleanDomain.ensureDecisionDomain(z.domain))
        NoPropagationOccurred.pruneDomains(x, dx1, y, dy1, z, dz1)
    }
    override protected def enforce(lhs: OrderedDomain[Value], rhs: OrderedDomain[Value]) =
        valueTraits.domainPruner.ltRule(lhs, rhs)
    override protected def prohibit(lhs0: OrderedDomain[Value], rhs0: OrderedDomain[Value]) = {
        val (rhs1, lhs1) = valueTraits.domainPruner.leRule(rhs0, lhs0)
        (lhs1, rhs1)
    }
}

/**
 * @author Michael Marte
 *
 */
final class Le
    [Value <: OrderedValue[Value]]
    (id: Id[Constraint], override val maybeGoal: Option[Goal],
     x: OrderedVariable[Value], y: OrderedVariable[Value], z: BooleanVariable)
    (implicit valueTraits: OrderedValueTraits[Value])
    extends TernaryConstraint(id, x, y, z)
    with ReifiedBinaryConstraintPropagator[OrderedDomain[Value], OrderedDomain[Value]]
{
    override def toString = "le(%s, %s, %s)".format(x, y, z)
    override def op(a: Value, b: Value) = valueTraits.orderingCostModel.leViolation(a, b)
    override def propagate = {
        val (dx1, dy1, dz1) = propagate(x.domain, y.domain, BooleanDomain.ensureDecisionDomain(z.domain))
        NoPropagationOccurred.pruneDomains(x, dx1, y, dy1, z, dz1)
    }
    override protected def enforce(lhs: OrderedDomain[Value], rhs: OrderedDomain[Value]) =
        valueTraits.domainPruner.leRule(lhs, rhs)
    override protected def prohibit(lhs0: OrderedDomain[Value], rhs0: OrderedDomain[Value]) = {
        val (rhs1, lhs1) = valueTraits.domainPruner.ltRule(rhs0, lhs0)
        (lhs1, rhs1)
    }
}

/**
 * @author Michael Marte
 *
 */
final class Min
    [Value <: OrderedValue[Value]]
    (id: Id[Constraint], override val maybeGoal: Option[Goal],
     x: OrderedVariable[Value], y: OrderedVariable[Value], z: OrderedVariable[Value])
    (implicit valueTraits: OrderedValueTraits[Value])
    extends TernaryConstraint(id, x, y, z)
{
    override def toString = "%s = min(%s, %s)".format(z, x, y)
    override def op(a: Value, b: Value) = if (a < b) a else b
    override def propagate = {
        val (lhs1, dz1) = valueTraits.domainPruner.minRule(Seq(x.domain, y.domain), z.domain)
        val Seq(dx1, dy1) = lhs1.toSeq
        NoPropagationOccurred.pruneDomains(x, dx1, y, dy1, z, dz1)
    }
}

/**
 * @author Michael Marte
 *
 */
final class Max
    [Value <: OrderedValue[Value]]
    (id: Id[Constraint], override val maybeGoal: Option[Goal],
     x: OrderedVariable[Value], y: OrderedVariable[Value], z: OrderedVariable[Value])
    (implicit valueTraits: OrderedValueTraits[Value])
    extends TernaryConstraint(id, x, y, z)
{
    override def toString = "%s = max(%s, %s)".format(z, x, y)
    override def op(a: Value, b: Value) = if (a > b) a else b
    override def propagate = {
        val (lhs1, dz1) = valueTraits.domainPruner.maxRule(Seq(x.domain, y.domain), z.domain)
        val Seq(dx1, dy1) = lhs1.toSeq
        NoPropagationOccurred.pruneDomains(x, dx1, y, dy1, z, dz1)
    }
}
