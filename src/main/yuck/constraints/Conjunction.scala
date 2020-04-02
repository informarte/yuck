package yuck.constraints

import scala.collection._

import yuck.core._

/**
 * Implements n-ary conjunction on cost level (where 0 is true).
 *
 * @author Michael Marte
 */
final class Conjunction
    (id: Id[Constraint], override val maybeGoal: Option[Goal],
     val xs: immutable.Seq[BooleanVariable], y: BooleanVariable)
    extends Constraint(id)
{

    require(xs.toSet.size == xs.size)

    override def toString = "%s = and([%s])".format(y, xs.mkString(", "))
    override def inVariables = xs
    override def outVariables = List(y)

    private var sum = True
    private val effects = List(new ReusableMoveEffectWithFixedVariable(y))
    private val effect = effects.head

    override def propagate = {
        val lhs0 = xs.view.map(_.domain)
        val rhs0 = y.domain
        val (lhs1, rhs1) = BooleanDomainPruner.conjunctionRule(lhs0, rhs0)
        NoPropagationOccurred.pruneDomains(xs.iterator.zip(lhs1.iterator)).pruneDomain(y, rhs1)
    }

    override def initialize(now: SearchState) = {
        var violation = 0L
        for (x <- xs) {
            violation = safeAdd(violation, now.value(x).violation)
        }
        sum = BooleanValue.get(violation)
        effect.a = sum
        effects
    }

    override def consult(before: SearchState, after: SearchState, move: Move) = {
        effect.a = sum
        var violation = sum.violation
        for (x0 <- move) {
            val x = x0.asInstanceOf[BooleanVariable]
            violation = safeSub(safeAdd(violation, after.value(x).violation), before.value(x).violation)
        }
        effect.a = BooleanValue.get(violation)
        effects
    }

    override def commit(before: SearchState, after: SearchState, move: Move) = {
        sum = effect.a
        effects
    }

}
