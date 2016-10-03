package yuck.constraints

import yuck.core._

/**
 * Implements the ''lex_less_int'' constraint as specified by MiniZinc.
 *
 * @author Michael Marte
 */
final class LexLess
    [Value <: OrderedValue[Value]]
    (id: Id[Constraint], goal: Goal,
     xs: Seq[Variable[Value]], ys: Seq[Variable[Value]],
     costs: Variable[IntegerValue])
    (implicit val ord: Ordering[TraversableOnce[Value]])
    extends Constraint(id, goal)
{

    override def toString = "lex_less(%s, %s)".format(xs, ys)
    override def inVariables = xs.toIterator ++ ys.toIterator
    override def outVariables = List(costs)

    private val effects = List(new ReusableEffectWithFixedVariable[IntegerValue](costs))
    private val effect = effects.head

    override def initialize(now: SearchState) = {
        val c = ord.compare(xs.toIterator.map(now.value(_)), ys.toIterator.map(now.value(_)))
        effect.a = if (c < 0) Zero else IntegerValue.get(c + 1)
        effects
    }

    override def consult(before: SearchState, after: SearchState, move: Move) =
        initialize(after)

}
