package yuck.core

import scala.annotation.tailrec

/**
 * Objective for optimizing the value of a numerical variable.
 */
abstract class NumericalObjective
    [A <: NumericalValue[A], D <: NumericalDomain[A, D], X <: NumericalVariable[A, D, X]]
    (using typeTraits: NumericalTypeTraits[A, D, X])
    extends PrimitiveObjective
{

    override val x: X
    val maybeY: Option[X]

    final override def costs(searchState: SearchState): NumericalValue[A] = searchState.value(x)
    final override def isSolution(costs: Costs) = isGoodEnough(costs)

    final override def findActualObjectiveValue(space: Space, rootObjective: AnyObjective) = {
        val minimize = optimizationMode == OptimizationMode.Min
        val costsOnEntry = rootObjective.costs(space.searchState)
        def isFeasibleObjectiveValue(a: A): Boolean = {
            val move = new ChangeValue(space.nextMoveId(), x, a)
            val after = space.consult(move)
            val costsAfterMove = rootObjective.costs(after)
            (! rootObjective.isHigherThan(costsAfterMove, costsOnEntry))
        }
        @tailrec
        def search(dx: D): Option[A] =
            if dx.isEmpty
            then None
            else if dx.isSingleton
            then {
                val a = dx.singleValue
                if isFeasibleObjectiveValue(a) then Some(a) else None
            } else {
                // binary search
                val (left, right) = dx.bisect
                (isFeasibleObjectiveValue(left.ub), isFeasibleObjectiveValue(right.lb)) match {
                    case (true, true) => search(if minimize then left else right)
                    case (false, false) => search(if minimize then right else left)
                    case (true, false) => if minimize then search(left) else Some(left.ub)
                    case (false, true) => if minimize then Some(right.lb) else search(right)
                }
            }
        if space.isSearchVariable(x) && ! space.isImplicitlyConstrainedSearchVariable(x) then {
            // We look for a value of x that is compatible with the current search state
            // while all smaller (or greater, respectively) values are in conflict with it.
            val dx0 = x.domain
            val a = space.searchState.value(x)
            val dx1 = if minimize then dx0.boundFromAbove(a) else dx0.boundFromBelow(a)
            val maybeB = search(dx1)
            if maybeB.isDefined then {
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
        tighten(space, bound.asInstanceOf[A])

    private def tighten(space: Space, bound: A): Set[AnyVariable] = {
        if maybeY.isDefined then {
            val y = maybeY.get
            assert(! space.isChannelVariable(y))
            assert(! space.isImplicitlyConstrainedSearchVariable(y))
            if y.domain.contains(bound) then {
                val move = new ChangeValue(space.nextMoveId(), y, bound)
                space.consult(move)
                space.commit(move)
                Set(x)
            } else {
                Set.empty
            }
        } else {
            Set.empty
        }
    }

}
