package yuck.constraints

import yuck.core._

/**
 * @author Michael Marte
 *
 */
final class SetCardinality
    (id: Id[Constraint], override val maybeGoal: Option[Goal],
     a: IntegerSetVariable, b: IntegerVariable)
    extends BinaryConstraint(id, a, b)
{
    override def toString = "set_cardinality(%s, %s)".format(a, b)
    override def op(a: IntegerSetValue) = IntegerValue.get(a.set.size)
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
        if (b.set.isEmpty) False else BooleanValue.get(b.set.distanceTo(a))
    override def propagate = {
        if (BooleanDomain.ensureDecisionDomain(z.domain) == TrueDomain && y.domain.isSingleton) {
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
        BooleanValue.get(a.set.maybeResidueSize(b.set).getOrElse(1))
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
    override def toString = "set_intersection(%s, %s, %s)".format(x, y, z)
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
    override def toString = "set_union(%s, %s, %s)".format(x, y, z)
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
    override def toString = "set_difference(%s, %s, %s)".format(x, y, z)
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
    override def toString = "symmetrical_set_difference(%s, %s, %s)".format(x, y, z)
    override def op(a: IntegerSetValue, b: IntegerSetValue) =
        new IntegerSetValue(a.set.union(b.set).diff(a.set.intersect(b.set)))
}
