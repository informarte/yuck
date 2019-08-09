package yuck.core

import scala.math._

/**
 * Objective for optimizing the value of a numerical variable.
 *
 * @author Michael Marte
 */
abstract class NumericalObjective
    [Value <: NumericalValue[Value]]
    (val x: NumericalVariable[Value])
    (implicit valueTraits: NumericalValueTraits[Value])
    extends AnyObjective
{

    override final def costs(searchState: SearchState) = searchState.value(x)
    override final def isGoodEnough(costs: Costs) = isSolution(costs)

    private var deltaScale = 0.0
    private var sampleSize = 0.0
    protected def computeDelta(before: SearchState, after: SearchState): Double

    override final def assessMove(before: SearchState, after: SearchState) = {
        var delta = computeDelta(before, after)
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
        val costsBeforeTightenining = rootObjective.costs(space.searchState)
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
                (isFeasibleObjectiveValue(d1.ub), isFeasibleObjectiveValue(d2.lb)) match {
                    case (true, true) => findActualObjectiveValue(if (minimize) d1 else d2)
                    case (false, false) => findActualObjectiveValue(if (minimize) d2 else d1)
                    case (true, false) => if (minimize) findActualObjectiveValue(d1) else Some(d1.ub)
                    case (false, true) => if (minimize) Some(d2.lb) else findActualObjectiveValue(d2)
                }
            }
        def isFeasibleObjectiveValue(a: Value): Boolean = {
            val move = new ChangeValue(space.nextMoveId, x, a)
            val before = space.searchState
            val after = space.consult(move)
            val costsAfterMove = rootObjective.costs(after)
            ! rootObjective.isHigherThan(costsAfterMove, costsBeforeTightenining)
        }
        def propagateBound(d: NumericalDomain[Value]): Unit = {
            val move = new ChangeValue(space.nextMoveId, x, if (minimize) d.ub else d.lb)
            space.consult(move)
            space.commit(move)
            x.pruneDomain(d)
        }
        if (  space.isSearchVariable(x) &&
            ! space.directlyAffectedConstraints(x).exists(space.isImplicitConstraint))
        {
            // We look for a value of x that is compatible with the current search state
            // while all smaller (or greater, respectively) values are in conflict with it.
            val dx0 = x.domain
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
                    val costsAfterTightening = rootObjective.costs(bestFeasibleSearchState)
                    assert(! rootObjective.isHigherThan(costsAfterTightening, costsBeforeTightenining))
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
