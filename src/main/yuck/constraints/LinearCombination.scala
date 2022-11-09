package yuck.constraints

import scala.collection.*

import yuck.core.*

/**
 * @author Michael Marte
 *
 */
final class LinearCombination
    [V <: NumericalValue[V]]
    (id: Id[Constraint], override val maybeGoal: Option[Goal],
     val axs: immutable.Seq[AX[V]], y: NumericalVariable[V])
    (using valueTraits: NumericalValueTraits[V])
    extends Constraint(id)
{

    require(axs.iterator.map(_.x).toSet.size == axs.size)

    override def toString = "%s = sum([%s])".format(y, axs.mkString(", "))

    override def inVariables = axs.view.map(_.x)
    override def outVariables = List(y)

    private val x2ax = immutable.HashMap[AnyVariable, AX[V]]() ++ (axs.iterator.map(_.x).zip(axs.iterator))
    private var sum = valueTraits.zero
    private val effect = y.reuseableEffect

    override def propagate() = {
        val lhs0 = axs.view.map(ax => (ax.a, ax.x.domain))
        val rhs0 = y.domain
        val (lhs1, rhs1) = valueTraits.domainPruner.linEqRule(lhs0, rhs0)
        NoPropagationOccurred.pruneDomains(axs.iterator.map(_.x).zip(lhs1)).pruneDomain(y, rhs1)
    }

    override def initialize(now: SearchState) = {
        sum = valueTraits.zero
        for ((_, ax) <- x2ax) {
            sum += ax.a * now.value(ax.x)
        }
        effect.a = sum
        effect
    }

    override def consult(before: SearchState, after: SearchState, move: Move) = {
        effect.a = sum
        for (x0 <- move) {
            val ax = x2ax(x0)
            effect.a = effect.a.addAndSub(ax.a, after.value(ax.x), before.value(ax.x))
        }
        effect
    }

    override def commit(before: SearchState, after: SearchState, move: Move) = {
        sum = effect.a
        effect
    }

}
