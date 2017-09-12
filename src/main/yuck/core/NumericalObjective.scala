package yuck.core

import scala.math._

/**
 * Objective for optimizing the value of a numerical variable.
 *
 * @author Michael Marte
 */
abstract class NumericalObjective
    [Value <: NumericalValue[Value]]
    (x: Variable[Value])
    (implicit valueTraits: NumericalValueTraits[Value])
    extends AnyObjective
{

    override final def costs(searchState: SearchState) = searchState.value(x)
    override final def isGoodEnough(costs: Costs) = isSolution(costs)

    private var deltaScale = 0.0
    private var sampleSize = 0.0
    protected def computeDelta(before: SearchState, after: SearchState): Value

    override final def assessMove(before: SearchState, after: SearchState) = {
        var delta = computeDelta(before, after).toDouble
        if (delta != 0) {
            val sign = signum(delta)
            delta = abs(delta)
            // scale compression (log(1) = 0, so shift curve to the left)
            delta = log(0.1 + delta)
            // scale normalization (see http://math.stackexchange.com/questions/106700/incremental-averageing)
            sampleSize += 1
            deltaScale += (delta - deltaScale) / sampleSize
            delta /= deltaScale
            delta *= sign
        }
        delta
    }

    protected final def tighten
        (space: Space, rootObjective: AnyObjective, tighteningStep: Value):
        (SearchState, Option[AnyVariable]) =
    {
        val minimize = tighteningStep < valueTraits.zero
        val costsOfCurrentProposal = rootObjective.costs(space.searchState)
        def bound(d: NumericalDomain[Value], a: Value): NumericalDomain[Value] =
            if (minimize) d.boundFromAbove(a) else d.boundFromBelow(a)
        def findActualObjectiveValue(d: NumericalDomain[Value]): Option[Value] =
            if (d.isEmpty) None
            else if (d.isSingleton) {
                val a = d.singleValue
                if (isFeasibleObjectiveValue(a)) Some(a) else None
            } else {
                // binary search
                val (d1, d2) = d.bisect
                val (t1, t2) = (isFeasibleObjectiveValue(d1.ub), isFeasibleObjectiveValue(d2.lb))
                if (t1 && t2) findActualObjectiveValue(if (minimize) d1 else d2)
                else if (! t1 && ! t2) findActualObjectiveValue(if (minimize) d2 else d1)
                else Some(if (minimize) d2.lb else d1.ub)
            }
        def isFeasibleObjectiveValue(a: Value): Boolean = {
            val move = new ChangeValue(space.moveIdFactory.nextId, x, a)
            val before = space.searchState
            val after = space.consult(move)
            val costsAfterMove = rootObjective.costs(after)
            ! rootObjective.isHigherThan(costsAfterMove, costsOfCurrentProposal)
        }
        def propagateBound(d: NumericalDomain[Value]) {
            val move = new ChangeValue(space.moveIdFactory.nextId, x, if (minimize) d.ub else d.lb)
            space.consult(move)
            space.commit(move)
            x.pruneDomain(d)
        }
        if (  space.isSearchVariable(x) &&
            ! space.directlyAffectedConstraints(x).exists(space.isImplicit))
        {
            // We look for a value of x that is compatible with the current search state
            // while all smaller (or greater, respectively) values are in conflict with it.
            val dx0 = valueTraits.safeDowncast(x.domain)
            val a = space.searchState.value(x)
            val maybeB = findActualObjectiveValue(bound(dx0, a))
            if (maybeB.isDefined) {
                val b = maybeB.get
                val c = b + tighteningStep
                val dx1 = bound(dx0, c)
                // So b is the actual objective value and dx1 is the future domain of x.
                if (b == a && dx1.isEmpty) {
                    (space.searchState, None)
                } else {
                    // First we assign b to x.
                    if (b != a) {
                        propagateBound(bound(dx0, b))
                    }
                    // Then we capture the resulting search state.
                    val bestFeasibleSearchState = space.searchState.clone
                    // Finally we constrain x to drive search towards a better solution.
                    if (! dx1.isEmpty) {
                        propagateBound(dx1)
                    }
                    (bestFeasibleSearchState, Some(x))
                }
            } else {
                (space.searchState, None)
            }
        } else {
            (space.searchState, None)
        }
    }

}
