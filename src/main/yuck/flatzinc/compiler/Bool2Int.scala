package yuck.flatzinc.compiler

import yuck.core._
import yuck.constraints.{UnaryConstraint, BinaryConstraint}

/**
 * @author Michael Marte
 *
 */
final class Bool2Int1
    (id: Id[Constraint], goal: Goal,
     a: Variable[BooleanValue], b: Variable[IntegerValue])
    extends UnaryConstraint(id, goal, a, b)
{
    override def toString = "bool2int(%s, %s)".format(a, b)
    override def op(a: BooleanValue) = if (a.truthValue) One else Zero
}

/**
 * @author Michael Marte
 *
 */
final class Bool2Int2
    (id: Id[Constraint], goal: Goal,
     x: Variable[BooleanValue], y: Variable[IntegerValue], z: Variable[BooleanValue])
    extends BinaryConstraint(id, goal, x, y, z)
{
    override def toString = "bool2int(%s, %s, %s)".format(x, y, z)
    override def op(a: BooleanValue, b: IntegerValue) =
        if ((a.truthValue && b == One) || (! a.truthValue && b == Zero)) True else False
}
