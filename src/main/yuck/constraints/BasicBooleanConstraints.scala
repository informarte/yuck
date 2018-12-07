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
    extends BinaryConstraint(id, goal, x, y)
{
    override def toString = "not(%s, %s)".format(x, y)
    override def op(a: BooleanValue) = if (a.truthValue) False else True
    override def propagate = {
        val dx0 = x.domain.asInstanceOf[BooleanDomain]
        val dy0 = y.domain.asInstanceOf[BooleanDomain]
        val (dx1, dy1) = BooleanDomainPruner.ne(dx0, dy0)
        Variable.pruneDomains(x, dx1, y, dy1)
    }
}
