package yuck.flatzinc.compiler

import yuck.core._
import yuck.constraints.{UnaryConstraint, BinaryConstraint}

/**
 * @author Michael Marte
 *
 */
final class Bool2Int1
    (id: Id[Constraint], goal: Goal,
     a: Variable[IntegerValue], b: Variable[IntegerValue])
    extends UnaryConstraint(id, goal, a, b)
{
    override def toString = "bool2int(%s, %s)".format(a, b)
    override def op(a: IntegerValue) = if (a == Zero) One else Zero
}

/**
 * @author Michael Marte
 *
 */
final class Bool2Int2
    (id: Id[Constraint], goal: Goal,
     x: Variable[IntegerValue], y: Variable[IntegerValue], z: Variable[IntegerValue])
    extends BinaryConstraint(id, goal, x, y, z)
{
    override def toString = "bool2int(%s, %s, %s)".format(x, y, z)
    override def op(a: IntegerValue, b: IntegerValue) =
        if ((a == Zero && b == One) || (a > Zero && b == Zero)) Zero else One
}
