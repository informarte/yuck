package yuck.constraints

import scala.annotation.tailrec
import scala.collection.*

import yuck.core.*

/**
 * @author Michael Marte
 *
 * Used to implement the family of fzn_if_then_else_* constraints.
 *
 * Ignores the value of the last element of cs and assumes it to be true.
 */
final class IfThenElse
    [V <: AnyValue]
    (id: Id[Constraint], override val maybeGoal: Option[Goal],
     cs: immutable.IndexedSeq[BooleanVariable], xs: immutable.IndexedSeq[Variable[V]], y: Variable[V])
    (implicit valueTraits: ValueTraits[V])
    extends Constraint(id)
{

    require(cs.size == xs.size)
    require(cs.size >= 2)

    private val n = cs.size

    override def toString = "if_then_else([%s], [%s], %s)".format(cs.mkString(", "), xs.mkString(", "), y)

    override def inVariables = cs.view ++ xs
    override def outVariables = List(y)

    // propagate from the x[i] and y to the c[i]
    private def propagate1(effects: PropagationEffects): PropagationEffects = {
        // identify impossible cases
        def identifyImpossibleCase(i: Int): BooleanDomain =
            if (xs(i).domain.intersects(y.domain)) cs(i).domain else cs(i).domain.intersect(FalseDomain)
        val cds = (0 until n).view.map(identifyImpossibleCase).toBuffer
        // make sure we have a default case
        @tailrec
        def findDefaultCase(i: Int): Int =
            if (i < 0) i
            else {
                cds.update(i, cds(i).intersect(TrueDomain))
                if (cds(i).isSingleton) i else findDefaultCase(i - 1)
            }
        findDefaultCase(n - 1)
        // prune the domains of the c[i] up to the first c[j] which could become true
        @tailrec
        def findFeasibleCase(i: Int): Int =
            if (i == n) i
            else {
                effects.pruneDomain(cs(i), cds(i))
                if (cs(i).domain.contains(True)) i else findFeasibleCase(i + 1)
            }
        findFeasibleCase(0)
        effects
    }

    // propagate from the c[i] to the x[i] and y
    @tailrec
    private def propagate2(effects: PropagationEffects, i: Int): PropagationEffects = {
        if (i == n) {
            effects
        } else if (cs(i).domain.isSingleton) {
            if (cs(i).domain.singleValue.truthValue) {
                // y = xs(i)
                effects.pruneDomains(xs(i), y.domain, y, xs(i).domain)
            } else {
                // skip impossible case
                propagate2(effects, i + 1)
            }
        } else if (valueTraits == IntegerSetValueTraits) {
            // bail out because integer-set domains do not support the union operation
            effects
        } else {
            // constructive disjunction: propagate the union of the x[j] domains, j > i, to y
            effects.pruneDomain(
                y,
                (i until n).view.filter(i => cs(i).domain != FalseDomain)
                    .foldLeft(valueTraits.emptyDomain){case (u, i) => u.union(xs(i).domain)})
        }
    }

    override def propagate() = {
        propagate2(propagate1(NoPropagationOccurred), 0)
    }

    override def initialize(now: SearchState) = {
        var i = 0
        while (i < n && ! (i == n - 1 || now.value(cs(i)).truthValue)) {
            i += 1
        }
        y.reuseableEffect.a = now.value(xs(i))
        y.reuseableEffect
    }

    override def consult(before: SearchState, after: SearchState, move: Move) =
        initialize(after)

    override def commit(before: SearchState, after: SearchState, move: Move) =
        y.reuseableEffect

}
