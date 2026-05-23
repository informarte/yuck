package yuck.constraints

import scala.collection.*

import yuck.core.*

final class Eq
    [A <: Value[A], D <: Domain[A, D], X <: Variable[A, D, X]]
    (id: Id[Constraint], x: X, y: X, z: BooleanVariable)
    (using typeTraits: TypeTraits[A, D, X])
    extends TernaryConstraint(id, x, y, z)
    with ReifiedBinaryConstraintPropagator[D, D]
{
    override def toString = "eq(%s, %s, %s)".format(x, y, z)
    override def copy(replacements: Map[AnyVariable, AnyVariable]) =
        new Eq(id, x, y, replacements.getOrElse(z, z).asInstanceOf[BooleanVariable])
    override def op(a: A, b: A) = BooleanValue(typeTraits.costModel.eqViolation(a, b))
    override def propagate() = {
        val (dx1, dy1, dz1) = propagate(x.domain, y.domain, z.domain)
        NoPropagationOccurred.pruneDomains(x, dx1, y, dy1, z, dz1)
    }
    override protected def enforce(lhs: D, rhs: D) =
        typeTraits.domainPruner.eqRule(lhs, rhs)
    override protected def prohibit(lhs: D, rhs: D) =
        typeTraits.domainPruner.neRule(lhs, rhs)
}

final class Ne
    [A <: Value[A], D <: Domain[A, D], X <: Variable[A, D, X]]
    (id: Id[Constraint], x: X, y: X, z: BooleanVariable)
    (using typeTraits: TypeTraits[A, D, X])
    extends TernaryConstraint(id, x, y, z)
    with ReifiedBinaryConstraintPropagator[D, D]
{
    override def toString = "ne(%s, %s, %s)".format(x, y, z)
    override def copy(replacements: Map[AnyVariable, AnyVariable]) =
        new Ne(id, x, y, replacements.getOrElse(z, z).asInstanceOf[BooleanVariable])
    override def op(a: A, b: A) = BooleanValue(typeTraits.costModel.neViolation(a, b))
    override def propagate() = {
        val (dx1, dy1, dz1) = propagate(x.domain, y.domain, z.domain)
        NoPropagationOccurred.pruneDomains(x, dx1, y, dy1, z, dz1)
    }
    override protected def enforce(lhs: D, rhs: D) =
        typeTraits.domainPruner.neRule(lhs, rhs)
    override protected def prohibit(lhs: D, rhs: D) =
        typeTraits.domainPruner.eqRule(lhs, rhs)
}

final class Lt
    [A <: OrderedValue[A], D <: OrderedDomain[A, D], X <: OrderedVariable[A, D, X]]
    (id: Id[Constraint], x: X, y: X, z: BooleanVariable)
    (using typeTraits: OrderedTypeTraits[A, D, X])
    extends TernaryConstraint(id, x, y, z)
    with ReifiedBinaryConstraintPropagator[D, D]
{
    override def toString = "lt(%s, %s, %s)".format(x, y, z)
    override def copy(replacements: Map[AnyVariable, AnyVariable]) =
        new Lt(id, x, y, replacements.getOrElse(z, z).asInstanceOf[BooleanVariable])
    override def op(a: A, b: A) = BooleanValue(typeTraits.costModel.ltViolation(a, b))
    override def propagate() = {
        val (dx1, dy1, dz1) = propagate(x.domain, y.domain, z.domain)
        NoPropagationOccurred.pruneDomains(x, dx1, y, dy1, z, dz1)
    }
    override protected def enforce(lhs: D, rhs: D) =
        typeTraits.domainPruner.ltRule(lhs, rhs)
    override protected def prohibit(lhs0: D, rhs0: D) = {
        val (rhs1, lhs1) = typeTraits.domainPruner.leRule(rhs0, lhs0)
        (lhs1, rhs1)
    }
}

final class Le
    [A <: OrderedValue[A], D <: OrderedDomain[A, D], X <: OrderedVariable[A, D, X]]
    (id: Id[Constraint], x: X, y: X, z: BooleanVariable)
    (using typeTraits: OrderedTypeTraits[A, D, X])
    extends TernaryConstraint(id, x, y, z)
    with ReifiedBinaryConstraintPropagator[D, D]
{
    override def toString = "le(%s, %s, %s)".format(x, y, z)
    override def copy(replacements: Map[AnyVariable, AnyVariable]) =
        new Le(id, x, y, replacements.getOrElse(z, z).asInstanceOf[BooleanVariable])
    override def op(a: A, b: A) = BooleanValue(typeTraits.costModel.leViolation(a, b))
    override def propagate() = {
        val (dx1, dy1, dz1) = propagate(x.domain, y.domain, z.domain)
        NoPropagationOccurred.pruneDomains(x, dx1, y, dy1, z, dz1)
    }
    override protected def enforce(lhs: D, rhs: D) =
        typeTraits.domainPruner.leRule(lhs, rhs)
    override protected def prohibit(lhs0: D, rhs0: D) = {
        val (rhs1, lhs1) = typeTraits.domainPruner.ltRule(rhs0, lhs0)
        (lhs1, rhs1)
    }
}

final class Min
    [A <: OrderedValue[A], D <: OrderedDomain[A, D], X <: OrderedVariable[A, D, X]]
    (id: Id[Constraint], x: X, y: X, z: X)
    (using typeTraits: OrderedTypeTraits[A, D, X])
    extends TernaryConstraint(id, x, y, z)
{
    override def toString = "%s = min(%s, %s)".format(z, x, y)
    override def copy(replacements: Map[AnyVariable, AnyVariable]) =
        new Min(id, x, y, replacements.getOrElse(z, z).asInstanceOf[X])
    override def op(a: A, b: A) = if a < b then a else b
    override def propagate() = {
        val (lhs1, dz1) = typeTraits.domainPruner.minRule(Seq(x.domain, y.domain), z.domain)
        val Seq(dx1, dy1) = lhs1.toSeq
        NoPropagationOccurred.pruneDomains(x, dx1, y, dy1, z, dz1)
    }
}

final class Max
    [A <: OrderedValue[A], D <: OrderedDomain[A, D], X <: OrderedVariable[A, D, X]]
    (id: Id[Constraint], x: X, y: X, z: X)
    (using typeTraits: OrderedTypeTraits[A, D, X])
    extends TernaryConstraint(id, x, y, z)
{
    override def toString = "%s = max(%s, %s)".format(z, x, y)
    override def copy(replacements: Map[AnyVariable, AnyVariable]) =
        new Max(id, x, y, replacements.getOrElse(z, z).asInstanceOf[X])
    override def op(a: A, b: A) = if a > b then a else b
    override def propagate() = {
        val (lhs1, dz1) = typeTraits.domainPruner.maxRule(Seq(x.domain, y.domain), z.domain)
        val Seq(dx1, dy1) = lhs1.toSeq
        NoPropagationOccurred.pruneDomains(x, dx1, y, dy1, z, dz1)
    }
}
