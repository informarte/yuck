package yuck.constraints

import yuck.core._

/**
 * Implements the ''lex_lesseq_int'' constraint as specified by MiniZinc.
 *
 * @author Michael Marte
 */
final class LexLessEq
    [Value <: OrderedValue[Value]]
    (id: Id[Constraint], goal: Goal,
     xs: Seq[OrderedVariable[Value]], ys: Seq[OrderedVariable[Value]],
     costs: BooleanVariable)
    (implicit val ord: Ordering[TraversableOnce[Value]])
    extends Constraint(id, goal)
{

    override def toString = "lex_lesseq(%s, %s)".format(xs, ys)
    override def inVariables = xs.toIterator ++ ys.toIterator
    override def outVariables = List(costs)

    private val effects = List(new ReusableEffectWithFixedVariable[BooleanValue](costs))
    private val effect = effects.head

    override def initialize(now: SearchState) = {
        val c = ord.compare(xs.toIterator.map(now.value(_)), ys.toIterator.map(now.value(_)))
        effect.a = if (c <= 0) True else BooleanValue.get(c)
        effects
    }

    override def consult(before: SearchState, after: SearchState, move: Move) =
        initialize(after)

}
