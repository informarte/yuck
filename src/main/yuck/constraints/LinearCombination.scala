package yuck.constraints

import scala.collection._

import yuck.core._

/**
 * @author Michael Marte
 *
 */
final class LinearCombination
    [Value <: NumericalValue[Value]]
    (id: Id[Constraint], goal: Goal,
     val axs: immutable.Seq[AX[Value]], y: NumericalVariable[Value])
    (implicit valueTraits: NumericalValueTraits[Value])
    extends Constraint(id, goal)
{

    require(axs.toIterator.map(_.x).toSet.size == axs.size)

    override def toString = "%s = sum([%s])".format(y, axs.mkString(", "))
    override def inVariables = axs.toIterator.map(_.x)
    override def outVariables = List(y)

    private val x2ax = immutable.HashMap[AnyVariable, AX[Value]]() ++ (axs.toIterator.map(_.x).zip(axs.toIterator))
    private var sum = valueTraits.zero
    private val effects = List(new ReusableMoveEffectWithFixedVariable[Value](y))
    private val effect = effects.head

    override def propagate = {
        val lhs0 = new Iterable[(Value, NumericalDomain[Value])] {
            override def iterator = axs.toIterator.map(ax => (ax.a, ax.x.domain))
        }
        val rhs0 = y.domain
        val (lhs1, rhs1) = valueTraits.domainPruner.linEq(lhs0, rhs0)
        Variable.pruneDomains(axs.toIterator.map(_.x).zip(lhs1.toIterator)) ||| y.pruneDomain(rhs1)
    }

    override def initialize(now: SearchState) = {
        sum = valueTraits.zero
        for ((_, ax) <- x2ax) {
            sum += ax.a * now.value(ax.x)
        }
        effect.a = sum
        effects
    }

    override def consult(before: SearchState, after: SearchState, move: Move) = {
        effect.a = sum
        for (x0 <- move) {
            val ax = x2ax(x0)
            effect.a = effect.a.addAndSub(ax.a, after.value(ax.x), before.value(ax.x))
        }
        effects
    }

    override def commit(before: SearchState, after: SearchState, move: Move) = {
        sum = effect.a
        effects
    }

}
