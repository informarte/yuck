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
    extends BinaryConstraint(id, goal, x, y, z)
{
    override def toString = "%s = %s == %s".format(z, x, y)
    override def op(x: Value, y: Value) = x.eqc(y)
}

/**
 * @author Michael Marte
 *
 */
final class Ne
    [Value <: OrderedValue[Value]]
    (id: Id[Constraint], goal: Goal,
     x: Variable[Value], y: Variable[Value], z: Variable[BooleanValue])
    extends BinaryConstraint(id, goal, x, y, z)
{
    override def toString = "%s = %s != %s".format(z, x, y)
    override def op(x: Value, y: Value) = x.nec(y)
}

/**
 * @author Michael Marte
 *
 */
final class Lt
    [Value <: OrderedValue[Value]]
    (id: Id[Constraint], goal: Goal,
     x: Variable[Value], y: Variable[Value], z: Variable[BooleanValue])
    extends BinaryConstraint(id, goal, x, y, z)
{
    override def toString = "%s = %s < %s".format(z, x, y)
    override def op(x: Value, y: Value) = x.ltc(y)
}

/**
 * @author Michael Marte
 *
 */
final class Le
    [Value <: OrderedValue[Value]]
    (id: Id[Constraint], goal: Goal,
     x: Variable[Value], y: Variable[Value], z: Variable[BooleanValue])
    extends BinaryConstraint(id, goal, x, y, z)
{
    override def toString = "%s = %s <= %s".format(z, x, y)
    override def op(x: Value, y: Value) = x.lec(y)
}

/**
 * @author Michael Marte
 *
 */
final class Min
    [Value <: NumericalValue[Value]]
    (id: Id[Constraint], goal: Goal,
     x: Variable[Value], y: Variable[Value], z: Variable[Value])
    extends BinaryConstraint(id, goal, x, y, z)
{
    override def toString = "%s = min(%s, %s)".format(z, x, y)
    override def op(a: Value, b: Value) = if (a < b) a else b
}

/**
 * @author Michael Marte
 *
 */
final class Max
    [Value <: NumericalValue[Value]]
    (id: Id[Constraint], goal: Goal,
     x: Variable[Value], y: Variable[Value], z: Variable[Value])
    extends BinaryConstraint(id, goal, x, y, z)
{
    override def toString = "%s = max(%s, %s)".format(z, x, y)
    override def op(a: Value, b: Value) = if (a > b) a else b
}
