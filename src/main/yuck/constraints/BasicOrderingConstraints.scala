package yuck.constraints

import yuck.core._

/**
 * @author Michael Marte
 *
 */
final class OrdEq
    [Value <: AnyValue]
    (id: Id[Constraint], goal: Goal,
     x: Variable[Value], y: Variable[Value], z: Variable[IntegerValue])
    extends BinaryConstraint(id, goal, x, y, z)
{
    override def toString = "%s = if %s == %s then %s else %s".format(z, x, y, Zero, One)
    override def op(x: Value, y: Value) = if (x == y) Zero else One
}

/**
 * @author Michael Marte
 *
 */
final class OrdNe
    [Value <: OrderedValue[Value]]
    (id: Id[Constraint], goal: Goal,
     x: Variable[Value], y: Variable[Value], z: Variable[IntegerValue])
    extends BinaryConstraint(id, goal, x, y, z)
{
    override def toString = "%s = if %s != %s then %s else %s".format(z, x, y, Zero, One)
    override def op(x: Value, y: Value) = if (x != y) Zero else One
}

/**
 * @author Michael Marte
 *
 */
final class OrdLt
    [Value <: OrderedValue[Value]]
    (id: Id[Constraint], goal: Goal,
     x: Variable[Value], y: Variable[Value], z: Variable[IntegerValue])
    extends BinaryConstraint(id, goal, x, y, z)
{
    override def toString = "%s = if %s < %s then %s else %s".format(z, x, y, Zero, One)
    override def op(x: Value, y: Value) = if (x < y) Zero else One
}

/**
 * @author Michael Marte
 *
 */
final class OrdLe
    [Value <: OrderedValue[Value]]
    (id: Id[Constraint], goal: Goal,
     x: Variable[Value], y: Variable[Value], z: Variable[IntegerValue])
    extends BinaryConstraint(id, goal, x, y, z)
{
    override def toString = "%s = if %s <= %s then %s else %s".format(z, x, y, Zero, One)
    override def op(x: Value, y: Value) = if (x <= y) Zero else One
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
