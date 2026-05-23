package yuck.constraints

import scala.annotation.tailrec
import scala.collection.*

import yuck.core.*

/**
 * Implements the ''lex_lesseq_int'' constraint as specified by MiniZinc.
 */
final class LexLessEq
    [A <: OrderedValue[A], D <: OrderedDomain[A, D], X <: OrderedVariable[A, D, X]]
    (id: Id[Constraint], val xs: immutable.IndexedSeq[X], val ys: immutable.IndexedSeq[X], val costs: BooleanVariable)
    (using val ord: Ordering[A])
    extends Constraint(id)
{

    override def toString = "lex_lesseq([%s], [%s])".format(xs.mkString(", "), ys.mkString(", "))

    override def copy(replacements: Map[AnyVariable, AnyVariable]) =
        new LexLessEq(id, xs, ys, replacements.getOrElse(costs, costs).asInstanceOf[BooleanVariable])(using ord)

    override def inVariables = xs.view ++ ys.view
    override def outVariables = List(costs)

    private val effect = new ReusableMoveEffectWithFixedVariable(costs)

    @tailrec
    private def findFailurePosition(searchState: SearchState, i: Int): Option[Int] =
        if i == xs.size
        then None
        else if i == ys.size
        then Some(i - 1)
        else {
            val a = searchState.value(xs(i))
            val b = searchState.value(ys(i))
            val cmp = ord.compare(a, b).sign
            if cmp < 0 then None else if cmp > 0 then Some(i) else findFailurePosition(searchState, i + 1)
        }

    override def initialize(now: SearchState) = {
        val maybeFailurePos = findFailurePosition(now, 0)
        effect.a = if maybeFailurePos.isEmpty then True else BooleanValue(min(xs.size, ys.size) - maybeFailurePos.get)
        effect
    }

    override def consult(before: SearchState, after: SearchState, move: Move) =
        initialize(after)

}
