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
     val axs: immutable.Seq[AX[Value]], y: Variable[Value])
    (implicit valueTraits: NumericalValueTraits[Value])
    extends Constraint(id, goal)
{

    override def toString = "%s = sum([%s])".format(y, axs.mkString(", "))
    override def inVariables = axs.toIterator.map(_.x)
    override def outVariables = List(y)

    private val id2ax = {
        val c = AX.compact(axs.toList)
        immutable.HashMap[AnyVariable, AX[Value]]() ++ (c.map(_.x).zip(c))
    }
    private var sum = valueTraits.zero
    private val effects = List(new ReusableEffectWithFixedVariable[Value](y))
    private val effect = effects.head

    override def initialize(now: SearchState) = {
        sum = valueTraits.zero
        for ((_, ax) <- id2ax) {
            sum = sum + ax.a * now.value(ax.x)
        }
        effect.a = sum
        effects
    }

    override def consult(before: SearchState, after: SearchState, move: Move) = {
        effect.a = sum
        for (x <- move.involvedVariables) {
            val ax = id2ax.get(x).get
            this.effect.a = this.effect.a + ax.a * (after.value(ax.x) - before.value(ax.x))
        }
        effects
    }

    override def commit(before: SearchState, after: SearchState, move: Move) = {
        sum = effect.a
        effects
    }

}
