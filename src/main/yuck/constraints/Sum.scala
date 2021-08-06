package yuck.constraints

import scala.collection._

import yuck.core._

/**
 * @author Michael Marte
 *
 */
final class Sum
    [Value <: NumericalValue[Value]]
    (id: Id[Constraint], override val maybeGoal: Option[Goal],
     val xs: immutable.Seq[NumericalVariable[Value]], y: NumericalVariable[Value])
    (implicit valueTraits: NumericalValueTraits[Value])
    extends Constraint(id)
{

    require(xs.toSet.size == xs.size)

    override def toString = "%s = sum([%s])".format(y, xs.mkString(", "))

    override def inVariables = xs
    override def outVariables = List(y)

    private var sum = valueTraits.zero
    private val effect = y.reuseableEffect

    override def propagate = {
        val lhs0 = new Iterable[(Value, NumericalDomain[Value])] {
            override def iterator = xs.iterator.map(x => (valueTraits.one, x.domain))
        }
        val rhs0 = y.domain
        val (lhs1, rhs1) = valueTraits.domainPruner.linEqRule(lhs0, rhs0)
        NoPropagationOccurred.pruneDomains(xs.iterator.zip(lhs1.iterator)).pruneDomain(y, rhs1)
    }

    override def initialize(now: SearchState) = {
        sum = valueTraits.zero
        for (x <- xs) {
            sum += now.value(x)
        }
        effect.a = sum
        effect
    }

    override def consult(before: SearchState, after: SearchState, move: Move) = {
        effect.a = sum
        for (x0 <- move) {
            val x = valueTraits.safeDowncast(x0)
            effect.a = effect.a.addAndSub(after.value(x), before.value(x))
        }
        effect
    }

    override def commit(before: SearchState, after: SearchState, move: Move) = {
        sum = effect.a
        effect
    }

}
