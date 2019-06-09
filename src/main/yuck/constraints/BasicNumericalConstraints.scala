package yuck.constraints

import yuck.core._

/**
 * @author Michael Marte
 *
 */
final class Plus
    [Value <: NumericalValue[Value]]
    (id: Id[Constraint], goal: Goal,
     x: NumericalVariable[Value], y: NumericalVariable[Value], z: NumericalVariable[Value])
    (implicit valueTraits: NumericalValueTraits[Value])
    extends TernaryConstraint(id, goal, x, y, z)
{
    override def toString = "%s = %s + %s".format(z, x, y)
    override def op(a: Value, b: Value) = a + b
    override def propagate = {
        import valueTraits.one
        val lhs0 = Seq((one, x.domain), (one, y.domain))
        val (lhs1, dz1) = valueTraits.domainPruner.linEq(lhs0, z.domain)
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
    (id: Id[Constraint], goal: Goal,
     x: NumericalVariable[Value], y: NumericalVariable[Value], z: NumericalVariable[Value])
    (implicit valueTraits: NumericalValueTraits[Value])
    extends TernaryConstraint(id, goal, x, y, z)
{
    override def toString = "%s = %s - %s".format(z, x, y)
    override def op(a: Value, b: Value) = a - b
    override def propagate = {
        import valueTraits.{one, zero}
        val lhs0 = Seq((one, x.domain), (zero - one, y.domain))
        val (lhs1, dz1) = valueTraits.domainPruner.linEq(lhs0, z.domain)
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
    (id: Id[Constraint], goal: Goal,
     x: NumericalVariable[Value], y: NumericalVariable[Value], z: NumericalVariable[Value])
    (implicit valueTraits: NumericalValueTraits[Value])
    extends TernaryConstraint[Value, Value, Value](id, goal, x, y, z)
{
    override def toString = "%s = %s * %s".format(z, x, y)
    override def op(a: Value, b: Value) = a * b
    override def propagate = {
        val (dx1, dy1, dz1) = valueTraits.domainPruner.times(x.domain, y.domain, z.domain)
        NoPropagationOccurred.pruneDomains(x, dx1, y, dy1, z, dz1)
    }
}

/**
 * @author Michael Marte
 *
 */
final class Div
    [Value <: NumericalValue[Value]]
    (id: Id[Constraint], goal: Goal,
     x: NumericalVariable[Value], y: NumericalVariable[Value], z: NumericalVariable[Value])
    extends TernaryConstraint(id, goal, x, y, z)
{
    override def toString = "%s = %s / %s".format(z, x, y)
    override def op(a: Value, b: Value) = a / b
}

/**
 * @author Michael Marte
 *
 */
final class Power
    [Value <: NumericalValue[Value]]
    (id: Id[Constraint], goal: Goal,
     x: NumericalVariable[Value], y: NumericalVariable[Value], z: NumericalVariable[Value])
    extends TernaryConstraint(id, goal, x, y, z)
{
    override def toString = "%s = %s ^ %s".format(z, x, y)
    override def op(a: Value, b: Value) = a ^ b
}

/**
 * @author Michael Marte
 *
 */
final class Mod
    [Value <: NumericalValue[Value]]
    (id: Id[Constraint], goal: Goal,
     x: NumericalVariable[Value], y: NumericalVariable[Value], z: NumericalVariable[Value])
    extends TernaryConstraint(id, goal, x, y, z)
{
    override def toString = "%s = %s % %s".format(z, x, y)
    override def op(a: Value, b: Value) = a % b
}

/**
 * @author Michael Marte
 *
 */
final class Abs
    [Value <: NumericalValue[Value]]
    (id: Id[Constraint], goal: Goal,
     x: NumericalVariable[Value], y: NumericalVariable[Value])
    extends BinaryConstraint(id, goal, x, y)
{
    override def toString = "%s = |%s|".format(y, x)
    override def op(a: Value) = a.abs
}

/**
 * @author Michael Marte
 *
 */
final class Even
    [Value <: NumericalValue[Value]]
    (id: Id[Constraint], goal: Goal,
     x: NumericalVariable[Value], y: BooleanVariable)
    extends BinaryConstraint(id, goal, x, y)
{
    override def toString = "%s = even(%s)".format(y, x)
    override def op(a: Value) = if (a.isEven) True else False
}

/**
 * @author Michael Marte
 *
 */
final class Uneven
    [Value <: NumericalValue[Value]]
    (id: Id[Constraint], goal: Goal,
     x: NumericalVariable[Value], y: BooleanVariable)
    extends BinaryConstraint(id, goal, x, y)
{
    override def toString = "%s = uneven(%s)".format(y, x)
    override def op(a: Value) = if (a.isEven) False else True
}
