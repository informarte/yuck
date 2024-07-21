package yuck.constraints

import scala.collection.*

import yuck.core.*

/**
 * @author Michael Marte
 *
 */
final class CountConst
    [V <: Value[V]]
    (id: Id[Constraint], override val maybeGoal: Option[Goal],
     xs: Seq[Variable[V]], a: V, n: IntegerVariable)
    (using valueTraits: ValueTraits[V])
    extends Constraint(id)
{

    require(valueTraits.normalizedValue(a) == a)

    override def toString = "%s = count(%s, [%s])".format(n, a, xs.mkString(", "))

    override def inVariables = xs
    override def outVariables = List(n)

    private val x2n: HashMap[AnyVariable, Int] =
        xs.groupBy(identity).view.mapValues(_.size).filter((_, n) => n > 1).to(HashMap)

    private var count = 0
    private val effect = n.reuseableEffect

    private def propagate1(effects: PropagationEffects): PropagationEffects = {
        val minCount = xs.count(x => x.domain.isSingleton && x.domain.contains(a))
        val maxCount = xs.count(_.domain.contains(a))
        effects.pruneDomain(n, IntegerRange(minCount, maxCount))
    }

    private def propagate2(effects: PropagationEffects): PropagationEffects = {
        if (valueTraits.domainCapabilities.createDomain &&
               n.domain.isSingleton &&
               xs.count(_.domain.contains(a)) == n.domain.singleValue.value)
        {
            val dx = valueTraits.createDomain(Set(a))
            xs.iterator.filter(_.domain.contains(a)).foldLeft(effects)((effects, x) => effects.pruneDomain(x, dx))
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
