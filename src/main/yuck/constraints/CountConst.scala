package yuck.constraints

import scala.collection._

import yuck.core._

/**
 * @author Michael Marte
 *
 */
final class CountConst
    [Value <: AnyValue]
    (id: Id[Constraint], goal: Goal,
     xs: Seq[Variable[Value]], a: Value, val n: IntegerVariable)
    extends Constraint(id, goal)
{

    override def toString = "%s = count(%s, [%s])".format(n, a, xs.mkString(", "))
    override def inVariables = xs
    override def outVariables = List(n)

    private var count = 0
    private val effects = List(new ReusableEffectWithFixedVariable[IntegerValue](n))
    private val effect = effects.head

    override def initialize(now: SearchState) = {
        count = xs.count(x => now.value(x) == a)
        effect.a = IntegerValue.get(count)
        effects
    }

    override def consult(before: SearchState, after: SearchState, move: Move) = {
        effect.a = IntegerValue.get(count + computeDelta(before, after, move))
        effects
    }

    override def commit(before: SearchState, after: SearchState, move: Move) = {
        count = effect.a.value
        effects
    }

    private def computeDelta(before: SearchState, after: SearchState, move: Move): Int = {
        var delta = 0
        for (x <- move.involvedVariables) {
            if (before.anyValue(x) == a && after.anyValue(x) != a) {
                delta -= 1
            } else if (before.anyValue(x) != a && after.anyValue(x) == a) {
                delta += 1
            }
        }
        delta
    }

}
