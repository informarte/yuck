package yuck.constraints

import yuck.core._

/**
 * Equality on cost level (where 0 is true)
 *
 * @author Michael Marte
 */
final class BoolEq
    (id: Id[Constraint], goal: Goal,
     x: Variable[IntegerValue], y: Variable[IntegerValue], z: Variable[IntegerValue])
    extends BinaryConstraint(id, goal, x, y, z)
{
    override def toString = "bool_eq(%s, %s, %s)".format(x, y, z)
    override def op(a: IntegerValue, b: IntegerValue) =
        if ((a == Zero && b == Zero) || (a > Zero && b > Zero)) Zero
        else IntegerValue.get(safeInc(safeAdd(a.value, b.value)) / 2)
}

/**
 * Inequality (negation) on cost level (where 0 is true)
 *
 * @author Michael Marte
 */
final class BoolNe
    (id: Id[Constraint], goal: Goal,
     x: Variable[IntegerValue], y: Variable[IntegerValue], z: Variable[IntegerValue])
    extends BinaryConstraint(id, goal, x, y, z)
{
    override def toString = "bool_ne(%s, %s, %s)".format(x, y, z)
    override def op(a: IntegerValue, b: IntegerValue) =
        if (a == Zero && b == Zero) One
        else if (a > Zero && b > Zero) IntegerValue.get(safeAdd(a.value, b.value) / 2)
        else Zero
}

/**
 * Implication on cost level (where 0 is true)
 *
 * @author Michael Marte
 */
final class BoolLe
    (id: Id[Constraint], goal: Goal,
     x: Variable[IntegerValue], y: Variable[IntegerValue], z: Variable[IntegerValue])
    extends BinaryConstraint(id, goal, x, y, z)
{
    override def toString = "bool_le(%s, %s, %s)".format(x, y, z)
    override def op(a: IntegerValue, b: IntegerValue) =
        if (a == Zero) IntegerValue.get(safeInc(b.value) / 2) else Zero
}

/**
 * (!x & y) on cost level (where 0 is true)
 *
 * @author Michael Marte
 */
final class BoolLt
    (id: Id[Constraint], goal: Goal,
     x: Variable[IntegerValue], y: Variable[IntegerValue], z: Variable[IntegerValue])
    extends BinaryConstraint(id, goal, x, y, z)
{
    override def toString = "bool_lt(%s, %s, %s)".format(x, y, z)
    override def op(a: IntegerValue, b: IntegerValue) =
        if (a == Zero) IntegerValue.get(safeInc(b.value)) else b
}

/**
 * Negation on cost level (where 0 is true)
 *
 * @author Michael Marte
 */
final class BoolNot
    (id: Id[Constraint], goal: Goal,
     x: Variable[IntegerValue], y: Variable[IntegerValue])
    extends UnaryConstraint(id, goal, x, y)
{
    override def toString = "bool_not(%s, %s)".format(x, y)
    override def op(a: IntegerValue) = if (a == Zero) One else Zero
}
