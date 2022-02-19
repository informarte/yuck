package yuck.constraints

import yuck.core.*

/**
 * @author Michael Marte
 *
 */
final class SetCardinality
    (id: Id[Constraint], override val maybeGoal: Option[Goal],
     x: IntegerSetVariable, y: IntegerVariable)
    extends BinaryConstraint(id, x, y)
{
    override def toString = "%s = set_cardinality(%s)".format(y, x)
    override def op(a: IntegerSetValue) = IntegerValue(a.set.size)
}

/**
 * @author Michael Marte
 *
 */
final class Contains
    (id: Id[Constraint], override val maybeGoal: Option[Goal],
     x: IntegerVariable, y: IntegerSetVariable, z: BooleanVariable)
    extends TernaryConstraint(id, x, y, z)
{
    override def toString = "contains(%s, %s, %s)".format(x, y, z)
    override def op(a: IntegerValue, b: IntegerSetValue) =
        if (b.set.isEmpty) False else BooleanValue(b.set.distanceTo(a).toLong)
    override def propagate() = {
        if (z.domain == TrueDomain && y.domain.isSingleton) {
            NoPropagationOccurred.pruneDomain(x, x.domain.intersect(y.domain.singleValue.set))
        } else {
            NoPropagationOccurred
        }
    }
}

/**
 * @author Michael Marte
 *
 */
final class Subset
    (id: Id[Constraint], override val maybeGoal: Option[Goal],
     x: IntegerSetVariable, y: IntegerSetVariable, z: BooleanVariable)
    extends TernaryConstraint(id, x, y, z)
{
    override def toString = "subset(%s, %s, %s)".format(x, y, z)
    override def op(a: IntegerSetValue, b: IntegerSetValue) =
        BooleanValue(a.set.maybeResidueSize(b.set).getOrElse(1))
}

/**
 * @author Michael Marte
 *
 */
final class SetIntersection
    (id: Id[Constraint], override val maybeGoal: Option[Goal],
     x: IntegerSetVariable, y: IntegerSetVariable, z: IntegerSetVariable)
    extends TernaryConstraint(id, x, y, z)
{
    override def toString = "%s = set_intersection(%s, %s)".format(z, x, y)
    override def op(a: IntegerSetValue, b: IntegerSetValue) = new IntegerSetValue(a.set.intersect(b.set))
}

/**
 * @author Michael Marte
 *
 */
final class SetUnion
    (id: Id[Constraint], override val maybeGoal: Option[Goal],
     x: IntegerSetVariable, y: IntegerSetVariable, z: IntegerSetVariable)
    extends TernaryConstraint(id, x, y, z)
{
    override def toString = "%s = set_union(%s, %s)".format(z, x, y)
    override def op(a: IntegerSetValue, b: IntegerSetValue) = new IntegerSetValue(a.set.union(b.set))
}

/**
 * @author Michael Marte
 *
 */
final class SetDifference
    (id: Id[Constraint], override val maybeGoal: Option[Goal],
     x: IntegerSetVariable, y: IntegerSetVariable, z: IntegerSetVariable)
    extends TernaryConstraint(id, x, y, z)
{
    override def toString = "%s = set_difference(%s, %s)".format(z, x, y)
    override def op(a: IntegerSetValue, b: IntegerSetValue) = new IntegerSetValue(a.set.diff(b.set))
}

/**
 * @author Michael Marte
 *
 */
final class SymmetricalSetDifference
    (id: Id[Constraint], override val maybeGoal: Option[Goal],
     x: IntegerSetVariable, y: IntegerSetVariable, z: IntegerSetVariable)
    extends TernaryConstraint(id, x, y, z)
{
    override def toString = "%s = symmetrical_set_difference(%s, %s)".format(z, x, y)
    override def op(a: IntegerSetValue, b: IntegerSetValue) =
        new IntegerSetValue(a.set.union(b.set).diff(a.set.intersect(b.set)))
}
