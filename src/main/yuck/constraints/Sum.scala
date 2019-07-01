package yuck.constraints

import scala.collection._

import yuck.core._

/**
 * @author Michael Marte
 *
 */
final class Sum
    [Value <: NumericalValue[Value]]
    (id: Id[Constraint], goal: Goal,
     val xs: immutable.Seq[NumericalVariable[Value]], y: NumericalVariable[Value])
    (implicit valueTraits: NumericalValueTraits[Value])
    extends Constraint(id, goal)
{

    require(xs.toSet.size == xs.size)

    override def toString = "%s = sum([%s])".format(y, xs.mkString(", "))
    override def inVariables = xs
    override def outVariables = List(y)

    private var sum = valueTraits.zero
    private val effects = List(new ReusableMoveEffectWithFixedVariable[Value](y))
    private val effect = effects.head

    override def propagate = {
        val lhs0 = new Iterable[(Value, NumericalDomain[Value])] {
            override def iterator = xs.toIterator.map(x => (valueTraits.one, x.domain))
        }
        val rhs0 = y.domain
        val (lhs1, rhs1) = valueTraits.domainPruner.linEq(lhs0, rhs0)
        Variable.pruneDomains(xs.toIterator.zip(lhs1.toIterator)) ||| y.pruneDomain(rhs1)
    }

    override def initialize(now: SearchState) = {
        sum = valueTraits.zero
        for (x <- xs) {
            sum += now.value(x)
        }
        effect.a = sum
        effects
    }

    override def consult(before: SearchState, after: SearchState, move: Move) = {
        effect.a = sum
        for (x0 <- move) {
            val x = valueTraits.safeDowncast(x0)
            effect.a = effect.a.addAndSub(after.value(x), before.value(x))
        }
        effects
    }

    override def commit(before: SearchState, after: SearchState, move: Move) = {
        sum = effect.a
        effects
    }

}
