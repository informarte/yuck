package yuck.constraints

import yuck.core.*

final class SetCardinality
    (id: Id[Constraint], x: IntegerSetVariable, y: IntegerVariable)
    extends BinaryConstraint(id, x, y)
{
    override def toString = "%s = set_cardinality(%s)".format(y, x)
    override def op(a: IntegerSetValue) = IntegerValue(a.set.size)
}

final class Contains
    (id: Id[Constraint], x: IntegerVariable, y: IntegerSetVariable, z: BooleanVariable)
    extends TernaryConstraint(id, x, y, z)
{
    override def toString = "contains(%s, %s, %s)".format(x, y, z)
    override def op(a: IntegerValue, b: IntegerSetValue) =
        if b.set.isEmpty then False else BooleanValue(b.set.distanceTo(a).toLong)
    override def propagate() = {
        if z.domain == TrueDomain && y.domain.isSingleton
        then NoPropagationOccurred.pruneDomain(x, x.domain.intersect(y.domain.singleValue.set))
        else NoPropagationOccurred
    }
}

final class Subset
    (id: Id[Constraint], x: IntegerSetVariable, y: IntegerSetVariable, z: BooleanVariable)
    extends TernaryConstraint(id, x, y, z)
{
    override def toString = "subset(%s, %s, %s)".format(x, y, z)
    override def op(a: IntegerSetValue, b: IntegerSetValue) =
        BooleanValue(a.set.maybeResidueSize(b.set).getOrElse(1))
}

final class SetIntersection
    (id: Id[Constraint], x: IntegerSetVariable, y: IntegerSetVariable, z: IntegerSetVariable)
    extends TernaryConstraint(id, x, y, z)
{
    override def toString = "%s = set_intersection(%s, %s)".format(z, x, y)
    override def op(a: IntegerSetValue, b: IntegerSetValue) = new IntegerSetValue(a.set.intersect(b.set))
}

final class SetUnion
    (id: Id[Constraint], x: IntegerSetVariable, y: IntegerSetVariable, z: IntegerSetVariable)
    extends TernaryConstraint(id, x, y, z)
{
    override def toString = "%s = set_union(%s, %s)".format(z, x, y)
    override def op(a: IntegerSetValue, b: IntegerSetValue) = new IntegerSetValue(a.set.union(b.set))
}

final class SetDifference
    (id: Id[Constraint], x: IntegerSetVariable, y: IntegerSetVariable, z: IntegerSetVariable)
    extends TernaryConstraint(id, x, y, z)
{
    override def toString = "%s = set_difference(%s, %s)".format(z, x, y)
    override def op(a: IntegerSetValue, b: IntegerSetValue) = new IntegerSetValue(a.set.diff(b.set))
}

final class SymmetricalSetDifference
    (id: Id[Constraint], x: IntegerSetVariable, y: IntegerSetVariable, z: IntegerSetVariable)
    extends TernaryConstraint(id, x, y, z)
{
    override def toString = "%s = symmetrical_set_difference(%s, %s)".format(z, x, y)
    override def op(a: IntegerSetValue, b: IntegerSetValue) =
        new IntegerSetValue(a.set.union(b.set).diff(a.set.intersect(b.set)))
}
