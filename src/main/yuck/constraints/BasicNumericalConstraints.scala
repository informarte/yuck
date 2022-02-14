package yuck.constraints

import yuck.core.*

/**
 * @author Michael Marte
 *
 */
final class Plus
    [V <: NumericalValue[V]]
    (id: Id[Constraint], override val maybeGoal: Option[Goal],
     x: NumericalVariable[V], y: NumericalVariable[V], z: NumericalVariable[V])
    (implicit valueTraits: NumericalValueTraits[V])
    extends TernaryConstraint(id, x, y, z)
{
    override def toString = "%s = %s + %s".format(z, x, y)
    override def op(a: V, b: V) = a + b
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
    [V <: NumericalValue[V]]
    (id: Id[Constraint], override val maybeGoal: Option[Goal],
     x: NumericalVariable[V], y: NumericalVariable[V], z: NumericalVariable[V])
    (implicit valueTraits: NumericalValueTraits[V])
    extends TernaryConstraint(id, x, y, z)
{
    override def toString = "%s = %s - %s".format(z, x, y)
    override def op(a: V, b: V) = a - b
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
    [V <: NumericalValue[V]]
    (id: Id[Constraint], override val maybeGoal: Option[Goal],
     x: NumericalVariable[V], y: NumericalVariable[V], z: NumericalVariable[V])
    (implicit valueTraits: NumericalValueTraits[V])
    extends TernaryConstraint[V, V, V](id, x, y, z)
{
    override def toString = "%s = %s * %s".format(z, x, y)
    override def op(a: V, b: V) = a * b
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
    [V <: IntegralValue[V]]
    (id: Id[Constraint], override val maybeGoal: Option[Goal],
     x: NumericalVariable[V], y: NumericalVariable[V], z: NumericalVariable[V])
    extends TernaryConstraint(id, x, y, z)
{
    override def toString = "%s = %s / %s".format(z, x, y)
    override def op(a: V, b: V) = a / b
}

/**
 * @author Michael Marte
 *
 */
final class Mod
    [V <: IntegralValue[V]]
    (id: Id[Constraint], override val maybeGoal: Option[Goal],
     x: NumericalVariable[V], y: NumericalVariable[V], z: NumericalVariable[V])
    extends TernaryConstraint(id, x, y, z)
{
    override def toString = "%s = %s %% %s".format(z, x, y)
    override def op(a: V, b: V) = a % b
}

/**
 * @author Michael Marte
 *
 */
final class Power
    [V <: NumericalValue[V]]
    (id: Id[Constraint], override val maybeGoal: Option[Goal],
     x: NumericalVariable[V], y: NumericalVariable[V], z: NumericalVariable[V])
    extends TernaryConstraint(id, x, y, z)
{
    override def toString = "%s = %s ^ %s".format(z, x, y)
    override def op(a: V, b: V) = a ^ b
}

/**
 * @author Michael Marte
 *
 */
final class Abs
    [V <: NumericalValue[V]]
    (id: Id[Constraint], override val maybeGoal: Option[Goal],
     x: NumericalVariable[V], y: NumericalVariable[V])
    (implicit valueTraits: NumericalValueTraits[V])
    extends BinaryConstraint(id, x, y)
{
    override def toString = "%s = abs(%s)".format(y, x)
    override def op(a: V) = a.abs
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
    [V <: IntegralValue[V]]
    (id: Id[Constraint], override val maybeGoal: Option[Goal],
     x: NumericalVariable[V], y: BooleanVariable)
    extends BinaryConstraint(id, x, y)
{
    override def toString = "even(%s, %s)".format(x, y)
    override def op(a: V) = if (a.isEven) True else False
}

/**
 * @author Michael Marte
 *
 */
final class Uneven
    [V <: IntegralValue[V]]
    (id: Id[Constraint], override val maybeGoal: Option[Goal],
     x: NumericalVariable[V], y: BooleanVariable)
    extends BinaryConstraint(id, x, y)
{
    override def toString = "uneven(%s, %s)".format(x, y)
    override def op(a: V) = if (a.isEven) False else True
}
