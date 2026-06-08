package yuck.constraints

import scala.collection.*

import yuck.core.*

final class LinearCombination
    [A <: NumericalValue[A], D <: NumericalDomain[A, D], X <: NumericalVariable[A, D, X]] private
    (id: Id[Constraint],
     val axs: immutable.Seq[AX[A, D, X]], val y: X,
     x2ax: HashMap[AnyVariable, AX[A, D, X]])
    (using typeTraits: NumericalTypeTraits[A, D, X])
    extends Constraint(id)
{

    require(axs.iterator.map(_.x).toSet.size == axs.size)

    def this
        (id: Id[Constraint], axs: immutable.Seq[AX[A, D, X]], y: X)
        (using typeTraits: NumericalTypeTraits[A, D, X]) =
    {
        this(id, axs, y, axs.view.map(ax => ax.x -> ax).to(HashMap))
    }

    override def toString = "%s = sum([%s])".format(y, axs.mkString(", "))

    override def copy(replacements: Map[AnyVariable, AnyVariable]) =
        new LinearCombination(id, axs, replacements.getOrElse(y, y).asInstanceOf[X], x2ax)

    override def inVariables = axs.view.map(_.x)
    override def outVariables = List(y)

    private var sum = typeTraits.zero
    private val effect = new ReusableMoveEffectWithFixedVariable(y)

    override def propagate() = {
        val lhs0 = axs.view.map(ax => (ax.a, ax.x.domain))
        val rhs0 = y.domain
        val (lhs1, rhs1) = typeTraits.domainPruner.linEqRule(lhs0, rhs0)
        NoPropagationOccurred.pruneDomains(axs.iterator.map(_.x).zip(lhs1)).pruneDomain(y, rhs1)
    }

    override def initialize(now: SearchState) = {
        sum = typeTraits.zero
        for (_, ax) <- x2ax do {
            sum += ax.a * now.value(ax.x)
        }
        effect.a = sum
        effect
    }

    override def consult(before: SearchState, after: SearchState, move: Move) = {
        effect.a = sum
        for x0 <- move do {
            val ax = x2ax(x0)
            val x = ax.x
            effect.a = effect.a.addAndSub(ax.a, after.value(x), before.value(x))
        }
        effect
    }

    override def commit(before: SearchState, after: SearchState, move: Move) = {
        sum = effect.a
        effect
    }

}
