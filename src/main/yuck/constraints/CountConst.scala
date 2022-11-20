package yuck.constraints

import scala.collection.*

import yuck.core.{given, *}

/**
 * @author Michael Marte
 *
 */
final class CountConst
    [V <: Value[V]]
    (id: Id[Constraint], override val maybeGoal: Option[Goal],
     xs: Seq[Variable[V]], a: V, val n: IntegerVariable)
    (using valueTraits: ValueTraits[V])
    extends Constraint(id)
{

    require(valueTraits.normalizedValue(a) == a)

    override def toString = "%s = count(%s, [%s])".format(n, a, xs.mkString(", "))

    override def inVariables = xs
    override def outVariables = List(n)

    private val x2n =
        xs
        .iterator
        .foldLeft(new mutable.HashMap[AnyVariable, Int]) {
            case (map, x) =>
                map.updateWith(x)(maybeN => Some(maybeN.getOrElse(0) + 1))
                map
        }
        .iterator
        .filter{case (_, n) => n > 1}
        .toMap

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
        if (valueTraits == IntegerSetValueTraits) {
            // bail out because IntegerSetValueTraits.createDomain is not fully implemented
            effects
        } else if (n.domain.isSingleton &&
                   xs.count(_.domain.contains(a)) == n.domain.singleValue.value)
        {
            val dx = valueTraits.createDomain(Set(a))
            xs.iterator.filter(_.domain.contains(a)).foldLeft(effects){case (effects, x) => effects.pruneDomain(x, dx)}
        } else {
            effects
        }
    }

    override def propagate() = {
        propagate2(propagate1(NoPropagationOccurred))
    }

    override def initialize(now: SearchState) = {
        count = xs.count(x => valueTraits.normalizedValue(now.value(x)) == a)
        effect.a = IntegerValue(count)
        effect
    }

    override def consult(before: SearchState, after: SearchState, move: Move) = {
        effect.a = IntegerValue(count + computeDelta(before, after, move))
        effect
    }

    override def commit(before: SearchState, after: SearchState, move: Move) = {
        count = effect.a.toInt
        effect
    }

    private def computeDelta(before: SearchState, after: SearchState, move: Move): Int = {
        var delta = 0
        for (x <- move) {
            val valueBefore = valueTraits.normalizedValue(valueTraits.safeDowncast(before.value(x)))
            val valueAfter = valueTraits.normalizedValue(valueTraits.safeDowncast(after.value(x)))
            if (valueBefore == a && valueAfter != a) {
                delta -= x2n.getOrElse(x, 1)
            } else if (valueBefore != a && valueAfter == a) {
                delta += x2n.getOrElse(x, 1)
            }
        }
        delta
    }

}
