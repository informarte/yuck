package yuck.constraints

import yuck.core._

/**
 * @author Michael Marte
 *
 */
final class NumEq
    [Value <: NumericalValue[Value]]
    (id: Id[Constraint], goal: Goal,
     x: Variable[Value], y: Variable[Value], z: Variable[Value])
    extends BinaryConstraint(id, goal, x, y, z)
{
    override def toString = "%s = |%s - %s|".format(z, x, y)
    override def op(a: Value, b: Value) = (a - b).abs
}

/**
 * @author Michael Marte
 *
 */
final class NumNe
    [Value <: NumericalValue[Value]]
    (id: Id[Constraint], goal: Goal,
     x: Variable[Value], y: Variable[Value], z: Variable[Value])
    (implicit valueTraits: NumericalValueTraits[Value])
    extends BinaryConstraint(id, goal, x, y, z)
{
    override def toString = "%s = if %s != %s then %s else %s".format(z, x, y, valueTraits.zero, valueTraits.one)
    override def op(x: Value, y: Value) = if (x != y) valueTraits.zero else valueTraits.one
}

/**
 * @author Michael Marte
 *
 */
final class NumLt
    [Value <: NumericalValue[Value]]
    (id: Id[Constraint], goal: Goal,
     x: Variable[Value], y: Variable[Value], z: Variable[Value])
    (implicit valueTraits: NumericalValueTraits[Value])
    extends BinaryConstraint(id, goal, x, y, z)
{
    override def toString =
        "%s = if %s < %s then %s else %s - %s + %s".format(z, x, y, valueTraits.zero, x, y, valueTraits.one)
    override def op(x: Value, y: Value) = if (x < y) valueTraits.zero else x - y + valueTraits.one
}

/**
 * @author Michael Marte
 *
 */
final class NumLe
    [Value <: NumericalValue[Value]]
    (id: Id[Constraint], goal: Goal,
     x: Variable[Value], y: Variable[Value], z: Variable[Value])
    (implicit valueTraits: NumericalValueTraits[Value])
    extends BinaryConstraint(id, goal, x, y, z)
{
    override def toString = "%s = if %s <= %s then %s else %s - %s".format(z, x, y, valueTraits.zero, x, y)
    override def op(a: Value, b: Value) = if (a <= b) valueTraits.zero else a - b
}

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
     x: Variable[Value], y: Variable[IntegerValue])
    extends UnaryConstraint(id, goal, x, y)
{
    override def toString = "%s = even(%s)".format(y, x)
    override def op(a: Value) = if (a.isEven) Zero else One
}

/**
 * @author Michael Marte
 *
 */
final class Uneven
    [Value <: NumericalValue[Value]]
    (id: Id[Constraint], goal: Goal,
     x: Variable[Value], y: Variable[IntegerValue])
    extends UnaryConstraint(id, goal, x, y)
{
    override def toString = "%s = uneven(%s)".format(y, x)
    override def op(a: Value) = if (a.isEven) One else Zero
}
