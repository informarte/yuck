package yuck.constraints

import yuck.core._

/**
 * Negation on cost level (where 0 is true)
 *
 * @author Michael Marte
 */
final class Not
    (id: Id[Constraint], goal: Goal,
     x: Variable[BooleanValue], y: Variable[BooleanValue])
    extends UnaryConstraint(id, goal, x, y)
{
    override def toString = "not(%s, %s)".format(x, y)
    override def op(a: BooleanValue) = if (a.truthValue) False else True
}
