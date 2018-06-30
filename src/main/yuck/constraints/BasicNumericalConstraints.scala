package yuck.constraints

import yuck.core._

/**
 * @author Michael Marte
 *
 */
final class Plus
    [Value <: NumericalValue[Value]]
    (id: Id[Constraint], goal: Goal,
     x: Variable[Value], y: Variable[Value], z: Variable[Value])
    extends BinaryConstraint(id, goal, x, y, z)
{
    override def toString = "%s = %s + %s".format(z, x, y)
    override def op(a: Value, b: Value) = a + b
}

/**
 * @author Michael Marte
 *
 */
final class Minus
    [Value <: NumericalValue[Value]]
    (id: Id[Constraint], goal: Goal,
     x: Variable[Value], y: Variable[Value], z: Variable[Value])
    extends BinaryConstraint(id, goal, x, y, z)
{
    override def toString = "%s = %s - %s".format(z, x, y)
    override def op(a: Value, b: Value) = a - b
}

/**
 * @author Michael Marte
 *
 */
final class Times
    [Value <: NumericalValue[Value]]
    (id: Id[Constraint], goal: Goal,
     x: Variable[Value], y: Variable[Value], z: Variable[Value])
    extends BinaryConstraint(id, goal, x, y, z)
{
    override def toString = "%s = %s * %s".format(z, x, y)
    override def op(a: Value, b: Value) = a * b
}

/**
 * @author Michael Marte
 *
 */
final class Div
    [Value <: NumericalValue[Value]]
    (id: Id[Constraint], goal: Goal,
     x: Variable[Value], y: Variable[Value], z: Variable[Value])
    extends BinaryConstraint(id, goal, x, y, z)
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
     x: Variable[Value], y: Variable[Value], z: Variable[Value])
    extends BinaryConstraint(id, goal, x, y, z)
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
     x: Variable[Value], y: Variable[Value], z: Variable[Value])
    extends BinaryConstraint(id, goal, x, y, z)
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
     x: Variable[Value], y: Variable[Value])
    extends UnaryConstraint(id, goal, x, y)
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
     x: Variable[Value], y: Variable[BooleanValue])
    extends UnaryConstraint(id, goal, x, y)
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
     x: Variable[Value], y: Variable[BooleanValue])
    extends UnaryConstraint(id, goal, x, y)
{
    override def toString = "%s = uneven(%s)".format(y, x)
    override def op(a: Value) = if (a.isEven) False else True
}
