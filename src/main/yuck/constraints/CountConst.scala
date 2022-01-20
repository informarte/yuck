package yuck.constraints

import scala.collection._

import yuck.core._

/**
 * @author Michael Marte
 *
 */
final class CountConst
    [V <: AnyValue]
    (id: Id[Constraint], override val maybeGoal: Option[Goal],
     xs: Seq[Variable[V]], a: V, val n: IntegerVariable)
    (implicit valueTraits: ValueTraits[V])
    extends Constraint(id)
{

    override def toString = "%s = count(%s, [%s])".format(n, a, xs.mkString(", "))

    override def inVariables = xs
    override def outVariables = List(n)

    private var count = 0
    private val effect = n.reuseableEffect

    private def propagate1(effects: PropagationEffects): PropagationEffects = {
        var lb = 0
        var ub = 0
        for (x <- xs) {
            val dx = x.domain
            if (dx.isSingleton) {
                if (dx.singleValue == a) {
                    lb += 1
                    ub += 1
                }
            } else {
                if (dx.contains(a)) {
                    ub += 1
                }
            }
        }
        effects.pruneDomain(n, IntegerRange(lb, ub))
    }

    private def propagate2(effects: PropagationEffects): PropagationEffects = {
        if (n.domain.isSingleton &&
            xs.count(_.domain.contains(a)) == n.domain.singleValue.value)
        {
            val dx = valueTraits.createDomain(Set(a))
            xs.iterator.filter(_.domain.contains(a)).foldLeft(effects){case (effects, x) => effects.pruneDomain(x, dx)}
        } else {
            effects
        }
    }

    override def propagate = {
        propagate2(propagate1(NoPropagationOccurred))
    }

    override def initialize(now: SearchState) = {
        count = xs.count(x => now.value(x) == a)
        effect.a = IntegerValue(count)
        effect
    }

    override def consult(before: SearchState, after: SearchState, move: Move) = {
        effect.a = IntegerValue(count + computeDelta(before, after, move))
        effect
    }

    override def commit(before: SearchState, after: SearchState, move: Move) = {
        count = effect.a.value
        effect
    }

    private def computeDelta(before: SearchState, after: SearchState, move: Move): Int = {
        var delta = 0
        for (x <- move) {
            if (before.value(x) == a && after.value(x) != a) {
                delta -= 1
            } else if (before.value(x) != a && after.value(x) == a) {
                delta += 1
            }
        }
        delta
    }

}
