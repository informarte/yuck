package yuck.constraints

import yuck.core._

/**
 * Negation on cost level (where 0 is true)
 *
 * @author Michael Marte
 */
final class Not
    (id: Id[Constraint], goal: Goal,
     x: BooleanVariable, y: BooleanVariable)
    extends BinaryConstraint(id, goal, x, y)
{
    override def toString = "not(%s, %s)".format(x, y)
    override def op(a: BooleanValue) = if (a.truthValue) False else True
    override def propagate = {
        val dx0 = x.domain
        val dy0 = y.domain
        val (dx1, dy1) = BooleanDomainPruner.ne(dx0, dy0)
        NoPropagationOccurred.pruneDomains(x, dx1, y, dy1)
    }
}
