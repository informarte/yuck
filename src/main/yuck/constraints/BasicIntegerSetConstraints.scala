package yuck.constraints

import yuck.core._

/**
 * @author Michael Marte
 *
 */
final class SetEq
    (id: Id[Constraint], goal: Goal,
     x: Variable[IntegerSetValue], y: Variable[IntegerSetValue], z: Variable[IntegerValue])
    extends BinaryConstraint(id, goal, x, y, z)
{
    override def toString = "set_eq(%s, %s, %s)".format(x, y, z)
    override def op(a: IntegerSetValue, b: IntegerSetValue) =
        IntegerValue.get(a.set.maybeResidueSize(b.set).getOrElse(1) + b.set.maybeResidueSize(a.set).getOrElse(1))
}

/**
 * @author Michael Marte
 *
 */
final class SetCard
    (id: Id[Constraint], goal: Goal,
     a: Variable[IntegerSetValue], b: Variable[IntegerValue])
    extends UnaryConstraint(id, goal, a, b)
{
    override def toString = "set_card(%s, %s)".format(a, b)
    override def op(a: IntegerSetValue) = IntegerValue.get(a.set.size)
}

/**
 * @author Michael Marte
 *
 */
final class SetIn
    (id: Id[Constraint], goal: Goal,
     x: Variable[IntegerValue], y: Variable[IntegerSetValue], z: Variable[IntegerValue])
    extends BinaryConstraint(id, goal, x, y, z)
{
    override def toString = "set_in(%s, %s, %s)".format(x, y, z)
    override def op(a: IntegerValue, b: IntegerSetValue) =
        if (b.set.isEmpty) One else IntegerValue.get(b.set.distanceTo(a))
}

/**
 * @author Michael Marte
 *
 */
final class SetSubset
    (id: Id[Constraint], goal: Goal,
     x: Variable[IntegerSetValue], y: Variable[IntegerSetValue], z: Variable[IntegerValue])
    extends BinaryConstraint(id, goal, x, y, z)
{
    override def toString = "set_subset(%s, %s, %s)".format(x, y, z)
    override def op(a: IntegerSetValue, b: IntegerSetValue) =
        IntegerValue.get(a.set.maybeResidueSize(b.set).getOrElse(1))
}

/**
 * @author Michael Marte
 *
 */
final class SetIntersect
    (id: Id[Constraint], goal: Goal,
     x: Variable[IntegerSetValue], y: Variable[IntegerSetValue], z: Variable[IntegerSetValue])
    extends BinaryConstraint(id, goal, x, y, z)
{
    override def toString = "set_intersect(%s, %s, %s)".format(x, y, z)
    override def op(a: IntegerSetValue, b: IntegerSetValue) = new IntegerSetValue(a.set.intersect(b.set))
}

/**
 * @author Michael Marte
 *
 */
final class SetUnion
    (id: Id[Constraint], goal: Goal,
     x: Variable[IntegerSetValue], y: Variable[IntegerSetValue], z: Variable[IntegerSetValue])
    extends BinaryConstraint(id, goal, x, y, z)
{
    override def toString = "set_union(%s, %s, %s)".format(x, y, z)
    override def op(a: IntegerSetValue, b: IntegerSetValue) = new IntegerSetValue(a.set.union(b.set))
}

/**
 * @author Michael Marte
 *
 */
final class SetDifference
    (id: Id[Constraint], goal: Goal,
     x: Variable[IntegerSetValue], y: Variable[IntegerSetValue], z: Variable[IntegerSetValue])
    extends BinaryConstraint(id, goal, x, y, z)
{
    override def toString = "set_diff(%s, %s, %s)".format(x, y, z)
    override def op(a: IntegerSetValue, b: IntegerSetValue) = new IntegerSetValue(a.set.diff(b.set))
}

/**
 * @author Michael Marte
 *
 */
final class SetSymmetricalDifference
    (id: Id[Constraint], goal: Goal,
     x: Variable[IntegerSetValue], y: Variable[IntegerSetValue], z: Variable[IntegerSetValue])
    extends BinaryConstraint(id, goal, x, y, z)
{
    override def toString = "set_symdiff(%s, %s, %s)".format(x, y, z)
    override def op(a: IntegerSetValue, b: IntegerSetValue) =
        new IntegerSetValue(a.set.union(b.set).diff(a.set.intersect(b.set)))
}
