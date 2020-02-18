package yuck.core

/**
 * Objective for optimizing the value of a numerical variable.
 *
 * @author Michael Marte
 */
abstract class NumericalObjective
    [Value <: NumericalValue[Value]]
    (implicit valueTraits: NumericalValueTraits[Value])
    extends PrimitiveObjective
{

    override val x: NumericalVariable[Value]

    final override def costs(searchState: SearchState) = searchState.value(x)
    final override def isGoodEnough(costs: Costs) = isSolution(costs)

    protected final def tighten
        (space: Space, rootObjective: AnyObjective, tighteningStep: Value):
        TighteningResult =
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
        def constrainObjective(dx: NumericalDomain[Value]): Unit = {
            val move = new ChangeValue(space.nextMoveId, x, if (minimize) dx.ub else dx.lb)
            space.consult(move)
            space.commit(move)
            x.pruneDomain(dx)
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
                val dx1 = bound(dx0, b)
                val c = b + tighteningStep
                val dx2 = bound(dx0, c)
                // So b is the actual objective value and dx2 is the future domain of x.
                // First we assign b to x.
                constrainObjective(dx1)
                val costsAfterTightening = rootObjective.costs(space.searchState)
                assert(! rootObjective.isHigherThan(costsAfterTightening, costsBeforeTightenining))
                if (dx2.isEmpty) {
                    // x cannot be constrained any further.
                    TighteningResult(space.searchState, if (b == a) None else Some(x))
                } else {
                    // Then we capture the resulting search state.
                    val bestFeasibleSearchState = space.searchState.clone
                    // Finally we constrain x to drive search towards a better solution.
                    constrainObjective(dx2)
                    TighteningResult(bestFeasibleSearchState, Some(x))
                }
            } else {
                TighteningResult(space.searchState, None)
            }
        } else {
            TighteningResult(space.searchState, None)
        }
    }

}
