package yuck.constraints

import yuck.core._

/**
 * @author Michael Marte
 *
 */
final class Plus
    [Value <: NumericalValue[Value]]
    (id: Id[Constraint], override val maybeGoal: Option[Goal],
     x: NumericalVariable[Value], y: NumericalVariable[Value], z: NumericalVariable[Value])
    (implicit valueTraits: NumericalValueTraits[Value])
    extends TernaryConstraint(id, x, y, z)
{
    override def toString = "%s = %s + %s".format(z, x, y)
    override def op(a: Value, b: Value) = a + b
    override def propagate = {
        import valueTraits.one
        val lhs0 = Seq((one, x.domain), (one, y.domain))
        val (lhs1, dz1) = valueTraits.domainPruner.linEqRule(lhs0, z.domain)
        val Seq(dx1, dy1) = lhs1.toSeq
        NoPropagationOccurred.pruneDomains(x, dx1, y, dy1, z, dz1)
    }
}

/**
 * @author Michael Marte
 *
 */
final class Minus
    [Value <: NumericalValue[Value]]
    (id: Id[Constraint], override val maybeGoal: Option[Goal],
     x: NumericalVariable[Value], y: NumericalVariable[Value], z: NumericalVariable[Value])
    (implicit valueTraits: NumericalValueTraits[Value])
    extends TernaryConstraint(id, x, y, z)
{
    override def toString = "%s = %s - %s".format(z, x, y)
    override def op(a: Value, b: Value) = a - b
    override def propagate = {
        import valueTraits.{one, zero}
        val lhs0 = Seq((one, x.domain), (zero - one, y.domain))
        val (lhs1, dz1) = valueTraits.domainPruner.linEqRule(lhs0, z.domain)
        val Seq(dx1, dy1) = lhs1.toSeq
        NoPropagationOccurred.pruneDomains(x, dx1, y, dy1, z, dz1)
    }
}

/**
 * @author Michael Marte
 *
 */
final class Times
    [Value <: NumericalValue[Value]]
    (id: Id[Constraint], override val maybeGoal: Option[Goal],
     x: NumericalVariable[Value], y: NumericalVariable[Value], z: NumericalVariable[Value])
    (implicit valueTraits: NumericalValueTraits[Value])
    extends TernaryConstraint[Value, Value, Value](id, x, y, z)
{
    override def toString = "%s = %s * %s".format(z, x, y)
    override def op(a: Value, b: Value) = a * b
    override def propagate = {
        val (dx1, dy1, dz1) = valueTraits.domainPruner.timesRule(x.domain, y.domain, z.domain)
        NoPropagationOccurred.pruneDomains(x, dx1, y, dy1, z, dz1)
    }
}

/**
 * @author Michael Marte
 *
 */
final class Div
    [Value <: IntegralValue[Value]]
    (id: Id[Constraint], override val maybeGoal: Option[Goal],
     x: NumericalVariable[Value], y: NumericalVariable[Value], z: NumericalVariable[Value])
    extends TernaryConstraint(id, x, y, z)
{
    override def toString = "%s = %s / %s".format(z, x, y)
    override def op(a: Value, b: Value) = a / b
}

/**
 * @author Michael Marte
 *
 */
final class Mod
    [Value <: IntegralValue[Value]]
    (id: Id[Constraint], override val maybeGoal: Option[Goal],
     x: NumericalVariable[Value], y: NumericalVariable[Value], z: NumericalVariable[Value])
    extends TernaryConstraint(id, x, y, z)
{
    override def toString = "%s = %s % %s".format(z, x, y)
    override def op(a: Value, b: Value) = a % b
}

/**
 * @author Michael Marte
 *
 */
final class Power
    [Value <: NumericalValue[Value]]
    (id: Id[Constraint], override val maybeGoal: Option[Goal],
     x: NumericalVariable[Value], y: NumericalVariable[Value], z: NumericalVariable[Value])
    extends TernaryConstraint(id, x, y, z)
{
    override def toString = "%s = %s ^ %s".format(z, x, y)
    override def op(a: Value, b: Value) = a ^ b
}

/**
 * @author Michael Marte
 *
 */
final class Abs
    [Value <: NumericalValue[Value]]
    (id: Id[Constraint], override val maybeGoal: Option[Goal],
     x: NumericalVariable[Value], y: NumericalVariable[Value])
    (implicit valueTraits: NumericalValueTraits[Value])
    extends BinaryConstraint(id, x, y)
{
    override def toString = "%s = abs(%s)".format(y, x)
    override def op(a: Value) = a.abs
    override def propagate = {
        val (dx1, dy1) = valueTraits.domainPruner.absRule(x.domain, y.domain)
        NoPropagationOccurred.pruneDomains(x, dx1, y, dy1)
    }
}

/**
 * @author Michael Marte
 *
 */
final class Even
    [Value <: IntegralValue[Value]]
    (id: Id[Constraint], override val maybeGoal: Option[Goal],
     x: NumericalVariable[Value], y: BooleanVariable)
    extends BinaryConstraint(id, x, y)
{
    override def toString = "even(%s, %s)".format(x, y)
    override def op(a: Value) = if (a.isEven) True else False
}

/**
 * @author Michael Marte
 *
 */
final class Uneven
    [Value <: IntegralValue[Value]]
    (id: Id[Constraint], override val maybeGoal: Option[Goal],
     x: NumericalVariable[Value], y: BooleanVariable)
    extends BinaryConstraint(id, x, y)
{
    override def toString = "uneven(%s, %s)".format(x, y)
    override def op(a: Value) = if (a.isEven) False else True
}
