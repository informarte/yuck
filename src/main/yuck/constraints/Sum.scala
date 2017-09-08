package yuck.constraints

import scala.collection._

import yuck.core._

/**
 * @author Michael Marte
 *
 */
final class Sum
    [Value <: NumericalValue[Value]]
    (id: Id[Constraint],
     goal: Goal,
     val xs: immutable.Seq[Variable[Value]],
     y: Variable[Value])
    (implicit valueTraits: NumericalValueTraits[Value])
    extends Constraint(id, goal)
{

    override def toString = "%s = sum([%s])".format(y, xs.mkString(", "))
    override def inVariables = xs
    override def outVariables = List(y)

    private var sum = valueTraits.zero
    private val effects = List(new ReusableEffectWithFixedVariable[Value](y))
    private val effect = effects.head

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
        for (x <- move.involvedVariables) {
            val y = x.asInstanceOf[Variable[Value]]
            effect.a += after.value(y) - before.value(y)
        }
        effects
    }

    override def commit(before: SearchState, after: SearchState, move: Move) = {
        sum = effect.a
        effects
    }

}
