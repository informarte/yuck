package yuck.constraints

import yuck.core._

/**
 * @author Michael Marte
 *
 */
final class Eq
    [Value <: OrderedValue[Value]]
    (id: Id[Constraint], goal: Goal,
     x: Variable[Value], y: Variable[Value], z: Variable[BooleanValue])
    (implicit valueTraits: OrderedValueTraits[Value])
    extends TernaryConstraint(id, goal, x, y, z)
    with ReifiedBinaryConstraintPropagator[Value, Value]
{
    override def toString = "%s = %s == %s".format(z, x, y)
    override def op(a: Value, b: Value) = valueTraits.orderingCostModel.eq(a, b)
    override def propagate = propagate(x, y, z)
    override protected def enforce(lhs: Domain[Value], rhs: Domain[Value]) =
        valueTraits.domainPruner.eq(valueTraits.safeDowncast(lhs), valueTraits.safeDowncast(rhs))
    override protected def prohibit(lhs: Domain[Value], rhs: Domain[Value]) =
        valueTraits.domainPruner.ne(valueTraits.safeDowncast(lhs), valueTraits.safeDowncast(rhs))
}

/**
 * @author Michael Marte
 *
 */
final class Ne
    [Value <: OrderedValue[Value]]
    (id: Id[Constraint], goal: Goal,
     x: Variable[Value], y: Variable[Value], z: Variable[BooleanValue])
    (implicit valueTraits: OrderedValueTraits[Value])
    extends TernaryConstraint(id, goal, x, y, z)
    with ReifiedBinaryConstraintPropagator[Value, Value]
{
    override def toString = "%s = %s != %s".format(z, x, y)
    override def op(a: Value, b: Value) = valueTraits.orderingCostModel.ne(a, b)
    override def propagate = propagate(x, y, z)
    override protected def enforce(lhs: Domain[Value], rhs: Domain[Value]) =
        valueTraits.domainPruner.ne(valueTraits.safeDowncast(lhs), valueTraits.safeDowncast(rhs))
    override protected def prohibit(lhs: Domain[Value], rhs: Domain[Value]) =
        valueTraits.domainPruner.eq(valueTraits.safeDowncast(lhs), valueTraits.safeDowncast(rhs))
}

/**
 * @author Michael Marte
 *
 */
final class Lt
    [Value <: OrderedValue[Value]]
    (id: Id[Constraint], goal: Goal,
     x: Variable[Value], y: Variable[Value], z: Variable[BooleanValue])
    (implicit valueTraits: OrderedValueTraits[Value])
    extends TernaryConstraint(id, goal, x, y, z)
    with ReifiedBinaryConstraintPropagator[Value, Value]
{
    override def toString = "%s = %s < %s".format(z, x, y)
    override def op(a: Value, b: Value) = valueTraits.orderingCostModel.lt(a, b)
    override def propagate = propagate(x, y, z)
    override protected def enforce(lhs: Domain[Value], rhs: Domain[Value]) =
        valueTraits.domainPruner.lt(valueTraits.safeDowncast(lhs), valueTraits.safeDowncast(rhs))
    override protected def prohibit(lhs0: Domain[Value], rhs0: Domain[Value]) = {
        val (rhs1, lhs1) = valueTraits.domainPruner.le(valueTraits.safeDowncast(rhs0), valueTraits.safeDowncast(lhs0))
        (lhs1, rhs1)
    }
}

/**
 * @author Michael Marte
 *
 */
final class Le
    [Value <: OrderedValue[Value]]
    (id: Id[Constraint], goal: Goal,
     x: Variable[Value], y: Variable[Value], z: Variable[BooleanValue])
    (implicit valueTraits: OrderedValueTraits[Value])
    extends TernaryConstraint(id, goal, x, y, z)
    with ReifiedBinaryConstraintPropagator[Value, Value]
{
    override def toString = "%s = %s <= %s".format(z, x, y)
    override def op(a: Value, b: Value) = valueTraits.orderingCostModel.le(a, b)
    override def propagate = propagate(x, y, z)
    override protected def enforce(lhs: Domain[Value], rhs: Domain[Value]) =
        valueTraits.domainPruner.le(valueTraits.safeDowncast(lhs), valueTraits.safeDowncast(rhs))
    override protected def prohibit(lhs0: Domain[Value], rhs0: Domain[Value]) = {
        val (rhs1, lhs1) = valueTraits.domainPruner.lt(valueTraits.safeDowncast(rhs0), valueTraits.safeDowncast(lhs0))
        (lhs1, rhs1)
    }
}

/**
 * @author Michael Marte
 *
 */
final class Min
    [Value <: OrderedValue[Value]]
    (id: Id[Constraint], goal: Goal,
     x: Variable[Value], y: Variable[Value], z: Variable[Value])
    (implicit valueTraits: OrderedValueTraits[Value])
    extends TernaryConstraint(id, goal, x, y, z)
{
    override def toString = "%s = min(%s, %s)".format(z, x, y)
    override def op(a: Value, b: Value) = if (a < b) a else b
    override def propagate = {
        import valueTraits.{safeDowncast => cast}
        val (lhs1, dz1) = valueTraits.domainPruner.min(Seq(cast(x.domain), cast(y.domain)), cast(z.domain))
        val Seq(dx1, dy1) = lhs1.toSeq
        Variable.pruneDomains(x, dx1, y, dy1, z, dz1)
    }
}

/**
 * @author Michael Marte
 *
 */
final class Max
    [Value <: OrderedValue[Value]]
    (id: Id[Constraint], goal: Goal,
     x: Variable[Value], y: Variable[Value], z: Variable[Value])
    (implicit valueTraits: OrderedValueTraits[Value])
    extends TernaryConstraint(id, goal, x, y, z)
{
    override def toString = "%s = max(%s, %s)".format(z, x, y)
    override def op(a: Value, b: Value) = if (a > b) a else b
    override def propagate = {
        import valueTraits.{safeDowncast => cast}
        val (lhs1, dz1) = valueTraits.domainPruner.max(Seq(cast(x.domain), cast(y.domain)), cast(z.domain))
        val Seq(dx1, dy1) = lhs1.toSeq
        Variable.pruneDomains(x, dx1, y, dy1, z, dz1)
    }
}
