package yuck.core

import scala.annotation.tailrec

/**
 * Objective for optimizing the value of a numerical variable.
 *
 * @author Michael Marte
 */
abstract class NumericalObjective
    [V <: NumericalValue[V]]
    (using valueTraits: NumericalValueTraits[V])
    extends PrimitiveObjective
{

    override val x: NumericalVariable[V]
    val maybeY: Option[NumericalVariable[V]]

    final override def costs(searchState: SearchState): NumericalValue[V] = searchState.value(x)
    final override def isSolution(costs: Costs) = isGoodEnough(costs)

    final override def findActualObjectiveValue(space: Space, rootObjective: AnyObjective) = {
        val minimize = optimizationMode == OptimizationMode.Min
        val costsOnEntry = rootObjective.costs(space.searchState)
        def isFeasibleObjectiveValue(a: V): Boolean = {
            val move = new ChangeValue(space.nextMoveId(), x, a)
            val after = space.consult(move)
            val costsAfterMove = rootObjective.costs(after)
            (! rootObjective.isHigherThan(costsAfterMove, costsOnEntry))
        }
        @tailrec
        def search(dx: NumericalDomain[V]): Option[V] =
            if (dx.isEmpty) None
            else if (dx.isSingleton) {
                val a = dx.singleValue
                if (isFeasibleObjectiveValue(a)) Some(a) else None
            } else {
                // binary search
                val (left, right) = dx.bisect
                (isFeasibleObjectiveValue(left.ub), isFeasibleObjectiveValue(right.lb)) match {
                    case (true, true) => search(if (minimize) left else right)
                    case (false, false) => search(if (minimize) right else left)
                    case (true, false) => if (minimize) search(left) else Some(left.ub)
                    case (false, true) => if (minimize) Some(right.lb) else search(right)
                }
            }
        if (space.isSearchVariable(x) && ! space.isImplicitlyConstrainedSearchVariable(x)) {
            // We look for a value of x that is compatible with the current search state
            // while all smaller (or greater, respectively) values are in conflict with it.
            val dx0 = x.domain
            val a = space.searchState.value(x)
            val dx1 = if (minimize) dx0.boundFromAbove(a) else dx0.boundFromBelow(a)
            val maybeB = search(dx1)
            if (maybeB.isDefined) {
                val b = maybeB.get
                // So b is the actual objective value!
                val move = new ChangeValue(space.nextMoveId(), x, b)
                space.consult(move)
                space.commit(move)
                val finalCosts = rootObjective.costs(space.searchState)
                assert(! rootObjective.isHigherThan(finalCosts, costsOnEntry))
            }
        }
    }

    final override def tighten(space: Space, bound: AnyValue) =
        tighten(space, bound.asInstanceOf[V])

    private def tighten(space: Space, bound: V): Set[AnyVariable] = {
        if (maybeY.isDefined) {
            val y = maybeY.get
            assert(! space.isChannelVariable(y))
            assert(! space.isImplicitlyConstrainedSearchVariable(y))
            if (y.domain.contains(bound)) {
                val move = new ChangeValue(space.nextMoveId(), y, bound)
                space.consult(move)
                space.commit(move)
                val dy0 = y.domain
                val dy1 = optimizationMode match {
                    case OptimizationMode.Min => dy0.boundFromAbove(bound)
                    case OptimizationMode.Max => dy0.boundFromBelow(bound)
                }
                if (y.pruneDomain(dy1)) Set(y) else Set.empty
            } else {
                Set.empty
            }
        } else {
            Set.empty
        }
    }

}
