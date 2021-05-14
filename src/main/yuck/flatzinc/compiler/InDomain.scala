package yuck.flatzinc.compiler

import scala.collection._

import yuck.core._

/**
 * Equivalent to set_in(x, x.domain) for all x.
 *
 * @author Michael Marte
 */
final class InDomain
    (id: Id[Constraint], override val maybeGoal: Option[Goal],
     val xs: immutable.Iterable[IntegerVariable], costs: BooleanVariable)
    extends Constraint(id)
{

    require(xs.toSet.size == xs.size)

    override def toString = "domains([%s], %s)".format(xs.mkString(", "), costs)

    override def inVariables = xs
    override def outVariables = List(costs)

    private var currentViolation = 0L
    private var futureViolation = 0L

    override def initialize(now: SearchState) = {
        currentViolation = 0
        for (x <- xs) {
            currentViolation = safeAdd(currentViolation, x.domain.distanceTo(now.value(x)).value)
        }
        costs.reuseableEffect.a = BooleanValue(currentViolation)
        costs.reuseableEffect
    }

    override def consult(before: SearchState, after: SearchState, move: Move) = {
        futureViolation = currentViolation
        for (x0 <- move) {
            val x = x0.asInstanceOf[IntegerVariable]
            val dx = x.domain
            val delta = safeSub(dx.distanceTo(after.value(x)).value, dx.distanceTo(before.value(x)).value)
            futureViolation = safeAdd(futureViolation, delta)
        }
        costs.reuseableEffect.a = BooleanValue(futureViolation)
        costs.reuseableEffect
    }

    override def commit(before: SearchState, after: SearchState, move: Move) = {
        currentViolation = futureViolation
        costs.reuseableEffect
    }

}
