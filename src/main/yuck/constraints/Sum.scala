package yuck.constraints

import scala.collection.*

import yuck.core.*

final class Sum
    [A <: NumericalValue[A], D <: NumericalDomain[A, D], X <: NumericalVariable[A, D, X]]
    (id: Id[Constraint], override val maybeGoal: Option[Goal],
     val xs: immutable.Seq[X], y: X)
    (using typeTraits: NumericalTypeTraits[A, D, X])
    extends Constraint(id)
{

    require(xs.toSet.size == xs.size)

    override def toString = "%s = sum([%s])".format(y, xs.mkString(", "))

    override def inVariables = xs
    override def outVariables = List(y)

    private var sum = typeTraits.zero
    private val effect = y.reuseableEffect

    override def propagate() = {
        val lhs0 = xs.view.map(x => (typeTraits.one, x.domain))
        val rhs0 = y.domain
        val (lhs1, rhs1) = typeTraits.domainPruner.linEqRule(lhs0, rhs0)
        NoPropagationOccurred.pruneDomains(xs.iterator.zip(lhs1)).pruneDomain(y, rhs1)
    }

    override def initialize(now: SearchState) = {
        sum = typeTraits.zero
        for x <- xs do {
            sum += now.value(x)
        }
        effect.a = sum
        effect
    }

    override def consult(before: SearchState, after: SearchState, move: Move) = {
        effect.a = sum
        for x0 <- move do {
            val x = typeTraits.safeDowncast(x0)
            effect.a = effect.a.addAndSub(after.value(x), before.value(x))
        }
        effect
    }

    override def commit(before: SearchState, after: SearchState, move: Move) = {
        sum = effect.a
        effect
    }

}
