package yuck.constraints

import scala.collection._

import yuck.core._

/**
 * Implements disjunction on cost level (where 0 is true).
 *
 * Roughly speaking, Disjunction(X, y) with X = (x[1], ..., x[n]) computes y
 * from X as follows:
 *
 * {{{
 * min X = 0    y
 * true         0
 * false        avg (x[1], ..., x[n])
 * }}}
 *
 * Rationale: Say we have a disjunction of reified constraints and
 * let X denote the set of their cost variables. The disjunction is
 * satisfied when one x in X takes the value 0. However, when all
 * x in X take positive values, how far is the solution away? Without
 * further knowledge, we have to assume that all options are equally
 * likely. Hence, the average value seems to be a more appropriate
 * measure of violation than, say the minimum or the maximum: Using
 * the average smoothes the cost landscape and this way guides the
 * search towards a solution.
 *
 * @see [[yuck.Notation Notation]]
 *
 * @author Michael Marte
 */
final class Disjunction
    (id: Id[Constraint],
     goal: Goal,
     xs: immutable.Seq[BooleanVariable],
     y: BooleanVariable)
    extends Constraint(id, goal)
{

    require(! xs.isEmpty)
    require(xs.size <= Int.MaxValue)

    override def toString = "disjunction(%s, %s)".format(xs, y)
    override def inVariables = xs
    override def outVariables = List(y)

    private val n = xs.size
    private var sum = 0L
    private var trueCount = 0
    private var futureSum = 0L
    private var futureTrueCount = 0
    private val effects = List(new ReusableEffectWithFixedVariable(y))
    private val effect = effects.head

    override def propagate = {
        import BooleanDomain.ensureDecisionDomain
        val dy = ensureDecisionDomain(y.domain)
        if (dy == TrueDomain) {
            if (xs.forall(x => ensureDecisionDomain(x.domain) == FalseDomain)) {
                y.pruneDomain(EmptyBooleanDomain)
            } else {
                false
            }
        } else if (dy == FalseDomain) {
            Variable.pruneDomains(xs.view.map(x => (x, FalseDomain)))
        } else if (xs.exists(x => ensureDecisionDomain(x.domain) == TrueDomain)) {
            y.pruneDomain(TrueDomain)
        } else if (xs.forall(x => ensureDecisionDomain(x.domain) == FalseDomain)) {
            y.pruneDomain(FalseDomain)
        } else {
            false
        }
    }

    override def initialize(now: SearchState) = {
        sum = 0
        trueCount = 0
        for (x <- xs) {
            val a = now.value(x).violation
            sum = safeAdd(sum, a)
            if (a == 0) trueCount += 1
        }
        effect.a = if (trueCount > 0) True else BooleanValue.get(sum / n)
        effects
    }

    override def consult(before: SearchState, after: SearchState, move: Move) = {
        futureSum = sum
        futureTrueCount = trueCount
        for (x <- move) {
            val y = x.asInstanceOf[BooleanVariable]
            val a = before.value(y).violation
            val b = after.value(y).violation
            futureSum = safeAdd(futureSum, safeSub(b, a))
            if (a == 0 && b > 0) futureTrueCount -= 1
            else if (a > 0 && b == 0) futureTrueCount += 1
        }
        effect.a = if (futureTrueCount > 0) True else BooleanValue.get(futureSum / n)
        effects
    }

    override def commit(before: SearchState, after: SearchState, move: Move) = {
        sum = futureSum
        trueCount = futureTrueCount
        effects
    }

}
